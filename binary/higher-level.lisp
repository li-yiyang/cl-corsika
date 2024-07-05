(in-package :cl-corsika/binary)

;; Documentation:
;; This part defines higher level reader and inspect (pretty print)
;; functions for Corsika output binary.

;; Pretty Print

(defmethod print-object ((run run) stream)
  (let ((run-number (floor (run-header-run-number (run-header run))))
        (events     (length (run-events run)))
        (date       (floor (run-header-start-date (run-header run)))))
      (format stream "#CORSIKA(#~d, ~d events, ~d-~d-~d)"
              run-number events
              (floor date 10000)
              (mod (floor date 100) 100)
              (mod date 100))))

(defmethod print-object ((run-header run-header) stream)
  (let* ((run-number (truncate (run-header-run-number run-header)))
         (observation-level-info (run-header-observation-level-info run-header))
         (observation-panes (subseq (observation-info-heights observation-level-info) 0
                                    (truncate (observation-info-number observation-level-info))))
         (energy-spectrum (run-header-energy-spectrum run-header))
         (energy-cutoff   (run-header-energy-cutoff   run-header)))
    (format stream "#RUN-HEADER(#~d, obs.lev. [~{~f~^, ~}], E [~f, ~f, ~f], [had. ~f, mu ~f, em. ~f, γ ~f])"
            run-number
            (map 'list #'identity observation-panes)
            (energy-spectrum-lower-limit energy-spectrum)
            (energy-spectrum-upper-limit energy-spectrum)
            (energy-spectrum-slope       energy-spectrum)
            (energy-cutoff-hadron        energy-cutoff)
            (energy-cutoff-muon          energy-cutoff)
            (energy-cutoff-electron      energy-cutoff)
            (energy-cutoff-photon        energy-cutoff))))

(defmethod print-object ((run-end run-end) stream)
  (let* ((run-number (truncate (run-end-run-number run-end))))
    (format stream "#RUN-END(#~d)" run-number)))

(defmethod print-object ((event event) stream)
  (let ((event-number (floor (event-header-event-number (event-header event))))
        (particle-id  (corsika-particle-name
                       (event-header-particle-id  (event-header event))))
        (energy       (event-header-energy       (event-header event)))
        (datasize     (length (event-datablocks event)))
        (long         (length (event-long-blocks event))))
      (format stream "#EVENT(~d, ~d, ~f GeV, ~d datablocks, ~d long-blocks)"
              event-number particle-id energy
              datasize long)))

(defmethod print-object ((event-header event-header) stream)
  (let* ((p (event-header-momentum     event-header)))
    (format stream "#EVENT-HEADER(#~d, pid ~d, ~f GeV, [~f, ~f, ~f] GeV/c)"
            (truncate (event-header-event-number event-header))
            (corsika-particle-name
             (event-header-particle-id  event-header))
            (event-header-energy       event-header)
            (momentum-x p) (momentum-y p) (momentum-z p))))

(defmethod print-object ((event-end event-end) stream)
  (format stream "#EVENT-HEADER(#~d)"
          (truncate (event-end-event-number event-end))))

(defun %parse-particle-description (desc)
  "Return particle-id hadr. generation and no. of obs. level. "
  (let* ((desc            (truncate desc))
         (particle-id     (floor desc 1000))
         (hadr-generation (mod (floor desc 10) 100))
         (obs-level       (mod desc 10)))
    (if (or (= particle-id 95)
            (= particle-id 96))
        (values particle-id (+ (* hadr-generation 10) obs-level))
        (values particle-id hadr-generation obs-level))))

(defmethod print-object ((particle particle) stream)
  (let* ((desc (particle-description particle))
         (x    (particle-x           particle))
         (y    (particle-y           particle))
         (p    (particle-momentum    particle))
         (px   (momentum-x           p))
         (py   (momentum-y           p))
         (pz   (momentum-z           p))
         (dt   (particle-time        particle)))
    (multiple-value-bind (pid hadr obs-level)
        (%parse-particle-description desc)
      (format stream "#PARTICLE(~a, hadr.g ~d, (~,2f, ~,2f) m, (~f, ~f, ~f) GeV/c, ~f ns, #~d)"
              (corsika-particle-name pid) hadr
              (/ x 100) (/ y 100)
              px py pz dt
              obs-level))))

(defmethod print-object ((data-block particle-data-sub-block) stream)
  (format stream "#PARTICLE-DATA-SUB-BLOCK(~d particles)"
          (count-if (lambda (particle) (not (zerop (particle-description particle))))
                    (particle-data-sub-block-particles data-block))))

;; High Level Interface

(defun iter-over-event (fn corsika)
  "Map over corsika event with function `fn' on `corsika'. "
  (etypecase corsika
    ;; for RUN class, map over events
    (run   (dolist (event (run-events corsika))
             (funcall fn corsika)))
    ;; for EVENT class, funcall `fn' on corsika
    (event (funcall fn corsika))
    ;; for EVENT list (possibly), map over list
    (list  (dolist (event (run-events corsika))
             (funcall fn corsika)))))

(defun particle? (particle)
  "Test if `particle' is a valid particle. "
  (and (typep particle 'particle)
       (not (zerop (particle-description particle)))))

(defun iter-over-particles (fn event)
  "Iter over corsika particle data with function `fn' on `event'. "
  (etypecase event
    (event
     (loop for data-block in (event-datablocks event)
           for particles = (particle-data-sub-block-particles data-block)
           do (dotimes (i 39)
                (let ((particle (aref particles i)))
                  (when (particle? particle)
                    (funcall fn particle))))))
    (run
     (loop for evt in (run-events event) do
       (iter-over-particles fn evt)))))
