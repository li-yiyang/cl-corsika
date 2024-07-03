(in-package :cl-corsika/binary)

;;; Documentation:

;;; TODO:

;;; WARNING:
;; Currently, only WITHOUT THIN output is readed.

(declaim (inline #:rewind-file))
(defun rewind-file (fstream &optional (backsteps 1))
  "Rewind file stream `fstream' by `backsteps'."
  (file-position fstream (- (file-position fstream) backsteps)))

(defconstant +corsika-block-size+ 22932
  "The size of Corsika block. (WITHOUT THIN). ")

(let ((block-counter 0))
  (defun %read-sub-block (fstream)
    "Read and return sub-block from `fstream'. "
    (flet ((block-size-check ()
             (let ((size-id (read-type :int fstream)))
               (when (not (eq size-id +corsika-block-size+))
                 (error (format nil "Block size shall be ~D, rather than ~D. "
                                +corsika-block-size+ size-id))))))
      (cond ((zerop block-counter)
             (block-size-check)
             (setf block-counter 1)
             (%read-sub-block fstream))
            ;; remove +corsika-block-size+ at start or end of the block;
            ((= block-counter 22)
             (block-size-check)
             (setf block-counter 0)
             (%read-sub-block fstream))
            (t
             ;; determine next sub-block type to read, and read the sub-block
             (let* ((id-str (read-type :str fstream))
                    (type   (cond ((string= id-str "RUNH") :run-header)
                                  ((string= id-str "EVTH") :event-header)
                                  ((string= id-str "LONG") :longitudinal-sub-block)
                                  ((string= id-str "EVTE") :event-end)
                                  ((string= id-str "RUNE") :run-end)
                                  ;; TODO: Add support for data-sub-block identify
                                  (t :particle-data-sub-block))))
               ;; unread sub-block id
               (rewind-file fstream)

               ;; return sub-block and block-counter (for debug)
               (let ((sub-block (read-type type fstream)))
                 (incf block-counter)
                 (values sub-block block-counter)))))))
  
  (defun %reset-block-counter ()
    (setf block-counter 0)))

(defun %read-event (fstream header)
  "Read event from `fstream'. "
  (loop for sub-block = (%read-sub-block fstream)
        if (typep sub-block 'particle-data-sub-block)
          collect sub-block into datablocks
        if (typep sub-block 'longitudinal-sub-block)
          collect sub-block into long-blocks
        if (typep sub-block 'event-end)
          return (make-event :header header
                             :datablocks datablocks
                             :long-blocks long-blocks
                             :end sub-block)))

(defun %read-run (fstream header)
  "Read run from `fstream' with `header'. "
  (loop for sub-block = (%read-sub-block fstream)
        if (typep sub-block 'event-header)
          collect (%read-event fstream sub-block) into events
        if (typep sub-block 'run-end)
          return (make-run :header header
                           :events events
                           :end sub-block)))

(defun read-corsika (fstream)
  "Read Corsika binary output to a `run' struct."
  (%reset-block-counter)
  (%read-run fstream (%read-sub-block fstream)))

(defmacro with-open-corsika ((corsika filespec) &body body)
  "With corsika run structure as `corsika' from `filespec'."
  (with-gensyms (fstream)
    `(with-corsika-binary-output (,fstream ,filespec)
       (let ((,corsika (read-corsika ,fstream)))
         ,@body))))
