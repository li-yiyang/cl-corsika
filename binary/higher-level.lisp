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

(defmethod print-object ((event event) stream)
  (let ((event-number (floor (event-header-event-number (event-header event))))
        (particle-id  (floor (event-header-particle-id  (event-header event))))
        (energy       (event-header-energy       (event-header event)))
        (datasize     (length (event-datablocks event)))
        (long         (length (event-long-blocks event))))
      (format stream "#EVENT(~d, ~d, ~f GeV, ~d datablocks, ~d long-blocks)"
              event-number particle-id energy
              datasize long)))

(defmethod print-object ((particle particle) stream)
  (let* ((id (particle-description particle))
         (x  (particle-x           particle))
         (y  (particle-y           particle))
         (p  (particle-momentum    particle))
         (px (momentum-x           p))
         (py (momentum-y           p))
         (pz (momentum-z           p))
         (dt (particle-time        particle)))
    (format stream "#PARTICLE(~d, (~f, ~f) cm, (~f, ~f, ~f) GeV/c, ~f nsec)"
            (truncate id) x y px py pz dt)))

;; High Level Interface

(defun map-over-event (fn corsika)
  "Map over corsika event with function `fn' on `corsika'. "
  (etypecase corsika
    ;; for RUN class, map over events
    (run   (mapcar fn (run-events corsika)))
    ;; for EVENT class, funcall `fn' on corsika
    (event (list (funcall fn corsika)))
    ;; for EVENT list (possibly), map over list
    (list  (mapcar fn corsika))))


