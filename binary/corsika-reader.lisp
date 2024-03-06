(in-package :cl-corsika/binary)

;;; Documentation:

;;; TODO:

;;; WARNING:
;; Currently, only WITHOUT THIN output is readed.

(declaim (inline #:rewind-file))
(defun rewind-file (fstream &optional (backsteps 1))
  "Rewind file stream `fstream' by `backsteps'."
  (file-position fstream (- (file-position fstream) backsteps)))

(defclass corsika-block-reader ()
  ((counter    :initform 0)
   (block-size :initform 22932)
   (fstream    :initarg  :file-stream))
  (:documentation "A reader for Corsika blocks."))

(defgeneric read-sub-block (reader)
  (:documentation "Read out a sub-block with correct sub block type.
Return values: sub-block data and type of sub-block."))

(defmethod read-sub-block ((reader corsika-block-reader))
  (with-slots (counter block-size fstream) reader
    (flet ((block-size-check ()
             (let ((size-id (read-type :int fstream)))
               (when (not (eq size-id block-size))
                 (error (format nil "Block size shall be ~d, rather than ~d."
                                block-size size-id))))))
      ;; Check block size at each block start
      (when (zerop counter) (block-size-check))
      ;; Read sub-block in the block
      (let* ((id-str (read-type :str fstream))
             (type (cond ((string= id-str "RUNH") :run-header)
                         ((string= id-str "EVTH") :event-header)
                         ((string= id-str "LONG") :longitudinal-sub-block)
                         ((string= id-str "EVTE") :event-end)
                         ((string= id-str "RUNE") :run-end)
                         ;; TODO: Add support for data-sub-block identify
                         (t :particle-data-sub-block))))
        (rewind-file fstream)       ; unread subblock type identifier
        (incf counter)              ; mark for readed a subblock
        (let ((sub-block (read-type type fstream)))
          (when (eq counter 21)     ; if readed 21 subblock
            (block-size-check)      ; check block size at block end
            (setf counter 0))       ; reset counter to 0 for new block
          (values sub-block type))))))  ; return readed sub-block and type

(defgeneric read-corsika (reader)
  (:documentation "Read Corsika binary output to a `run' struct."))

(defmethod read-corsika ((reader corsika-block-reader))
  ;; NOTE: currently I think only one RUN per OUTPUT file
  ;; now the code is kinda like a sketch code... may need
  ;; revise later some times.
  (loop with sub-block = nil
        with type      = :run-header
        with run       = (make-run)
        do (multiple-value-setq (sub-block type)
             (read-sub-block reader))
        do (ecase type
             (:run-header
              (setf (run-header run) sub-block))
             (:run-end
              (setf (run-end run) sub-block))
             (:event-header
              (setf (run-events run)
                    (loop with event = (make-event :header sub-block)
                          with datablocks = nil
                          with long-blocks = nil
                          do (multiple-value-setq (sub-block type)
                               (read-sub-block reader))
                          do (ecase type
                               (:particle-data-sub-block
                                (push sub-block datablocks))
                               (:longitudinal-sub-block
                                (push sub-block long-blocks))
                               (:event-end
                                (setf (event-end event) sub-block)))
                          while (not (eq type :event-end))
                          finally
                             (setf
                              (event-datablocks event) (nreverse datablocks)
                              (event-long-blocks event) (nreverse long-blocks))))))
        finally (return run)))

(defmacro wrap-corsika-binary-reader ((reader stream) &body body)
  "Wrap existing corsika binary file stream into corsika-block-reader."
  (with-gensyms (fstream)
    `(let* ((,fstream ,stream)
            (,reader  (make-instance 'corsika-block-reader
                                     :file-stream ,fstream)))
       ,@body)))

(defmacro with-open-corsika ((corsika filespec) &body body)
  "With corsika run structure as `corsika' from `filespec'."
  (with-gensyms (fstream reader)
    `(with-corsika-binary-output (,fstream ,filespec)
       (wrap-corsika-binary-reader (,reader ,fstream)
         (let ((,corsika (read-corsika ,reader)))
           ,@body)))))
