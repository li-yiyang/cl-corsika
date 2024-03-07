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

(defgeneric next-sub-block-type (reader)
  (:documentation "Pre read next sub-block type.
Return values: type of sub-block.

Please note that this won't change the `fstream' position of `reader'."))

(defmethod next-sub-block-type ((reader corsika-block-reader))
  (with-slots (counter block-size fstream) reader
    ;; Ensure block size at each block start
    (when (zerop counter)
      (let ((size-id (read-type :int fstream)))
        (when (not (eq size-id block-size))
          (error (format nil "Block size shall be ~d, rather than ~d."
                         block-size size-id)))))
    ;; Preread next sub-block type
    (let* ((id-str (read-type :str fstream))
           (type   (cond ((string= id-str "RUNH") :run-header)
                         ((string= id-str "EVTH") :event-header)
                         ((string= id-str "LONG") :longitudinal-sub-block)
                         ((string= id-str "EVTE") :event-end)
                         ((string= id-str "RUNE") :run-end)
                         ;; TODO: Add support for data-sub-block identify
                         (t :particle-data-sub-block))))
      (rewind-file fstream)
      type)))

(defgeneric read-sub-block (reader &optional type)
  (:documentation "Read out a sub-block with correct sub block type.
Return values: sub-block data and type of sub-block."))

(defmethod read-sub-block ((reader corsika-block-reader)
                           &optional (type-check nil type-check-set-p))
  (with-slots (counter fstream) reader
    (let ((type (next-sub-block-type reader)))
      ;; Check next sub block type if set with `type-check'.
      (when (and type-check-set-p
                 (typecase type-check
                   (list (not (find type type-check)))
                   (otherwise (not (eq type type-check)))))
        (error (format nil
                       "Expecting ~a sub-block, but get ~a at posistion ~d."
                       type-check type (file-position fstream))))
      ;; Read next sub-block
      (let ((sub-block (read-type type fstream)))
        (incf counter)
        (when (eq counter 21)       ; if readed 21 subblock
          (read-type :int fstream)  ; read out block size...
          (setf counter 0))         ; reset counter to 0 for new block
        (values sub-block type))))) ; return readed sub-block and type

(defgeneric read-corsika (reader)
  (:documentation "Read Corsika binary output to a `run' struct."))

(defmethod read-corsika ((reader corsika-block-reader))
  ;; NOTE: currently I think only one RUN per OUTPUT file
  ;; now the code is kinda like a sketch code... may need
  ;; revise later some times.
  (with-slots (fstream) reader
    (flet ((read-event ()
             (loop with header = (read-sub-block reader :event-header)
                   with sub-block = nil
                   with type = nil
                   do (multiple-value-setq (sub-block type)
                        (read-sub-block reader
                                        '(:particle-data-sub-block
                                          :longitudinal-sub-block
                                          :event-end)))
                   if (eq :particle-data-sub-block type)
                     collect sub-block into datablocks
                   if (eq :longitudinal-sub-block type)
                     collect sub-block into long-blocks
                   while (not (eq :event-end type))
                   finally (return (make-event :header header
                                               :datablocks datablocks
                                               :long-blocks long-blocks
                                               :end sub-block)))))
      (loop with header = (read-sub-block reader :run-header)
            while (eq :event-header (next-sub-block-type reader))
            collect (read-event) into events
            finally (return (make-run :header header
                                      :events events
                                      :end (read-sub-block reader :run-end)))))))

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
