(in-package :cl-corsika/binary)

;;; Documentation:
;; This part defines `defbin' macro for easily define Corsika output
;; binary sub-block data structure with automatically generated reader.
;; 
;; The `defbin' macro needs to be simple to write:
;; 
;;   (defbin binary-type-name (&key options)
;;     slot-name ;; single name of slot is ok
;;     (slot-name :type type) ;; nested slot defination overwrites default config
;;     (:no-use   :offset offset-size)) ;; `:no-use' marks for space not used
;; 
;; By default, the default read out data type is `:float'; you could
;; change it in `options' using `:default-type' key. The default offset
;; for `:no-use' is `1'.

;;; TODO:
;; 1. More precised type identifier and better performance improve
;; 2. Add export support for method definition
;; 3. Add more utilities for `defbin'

;;; File Reader
(defmacro with-corsika-binary-output ((corsika filespec
                                       &key (if-does-not-exist :error))
                                      &body body)
  "Open Corsika binary output file."
  (declare (type symbol corsika))
  `(with-open-file (,corsika ,filespec :if-does-not-exist ,if-does-not-exist
                                       :element-type '(unsigned-byte 32))
     ,@body))

;; Corsika binary type readers
(defun complex-type-p (type)
  "Test if a `type' specifier is an complex type.
A complex type is like `(:array :float 10)' type."
  (listp type))

(defun default-type-value (type)
  "Map `type' to default `defstruct' type value."
  (if (complex-type-p type)
      (ecase (first type)
        (:array #()))
      (case type
        (:float 0.0)
        (:int   0)
        (:str   "")
        (otherwise nil))))

;; NOTE: Furture work: add type identifier and specifier for performance improve
(defun default-type-name (type)
  "Map `type' to default `defstruct' type name."
  (if (complex-type-p type)
      (ecase (first type)
        (:array 'array))
      (case type
        (:float 'single-float)
        (:int   '(unsigned-byte 32))
        (:str   'string)
        (otherwise t))))

(defparameter *corsika-binary-type-size*
  (make-hash-table)
  "Get corsika binary type size.")

(defun type-size (type &optional (size 1 size-set-p))
  "Get type size for `type', if provided `size', set type size."
  (if (complex-type-p type)
      (ecase (first type)
        (:array (* (type-size (second type)) (third type))))
      (let ((type (if (keywordp type) type (make-keyword type))))
        (if size-set-p
            (setf (gethash type *corsika-binary-type-size*) size)
            (let ((size (gethash type *corsika-binary-type-size* nil)))
              (if size size
                  (error (format nil "type ~a is not defined yet." type))))))))

;; Register basic type size in `*corsika-binary-type-size*'.
(type-size :float 1)
(type-size :int   1)
(type-size :str   1)

(defparameter *corsika-binary-readers*
  (make-hash-table)
  "Corsika and basic binary reader function table.")

(defun read-float (corsika &optional (eof-error-p t) eof-value)
  "Read float32 (single-float) from `corika'."
  (decode-float32 (read-byte corsika eof-error-p eof-value)))

(defun read-str (corsika &optional (eof-error-p t) eof-value)
  "Read str (4 char) from `corsika'."
  (let ((byte (read-byte corsika eof-error-p eof-value)))
    (map 'string #'code-char
         (loop for i below 4 collect (logand #xff (ash byte (* -8 i)))))))

;; Register basic type reader in `*corsika-binary-readers*'.
(setf (gethash :float *corsika-binary-readers*) #'read-float
      (gethash :int   *corsika-binary-readers*) #'read-byte
      (gethash :str   *corsika-binary-readers*) #'read-str)

;;; Main reader function
(defun read-type (type stream &optional (eof-error-p t) eof-value
                                (reader-table *corsika-binary-readers*))
  "Read `type' from `stream'."
  (if (complex-type-p type)
      (ecase (first type)
        (:array
         (let* ((dim       (third type))
                (elem-type (second type))
                (array     (make-array dim :element-type elem-type)))
           (declare (type integer dim))
           (dotimes (idx dim)
             (setf (aref array idx)
                   (read-type elem-type stream eof-error-p eof-value)))
           array)))
      (let ((fn (gethash (make-keyword type) reader-table)))
        (if (functionp fn)
            (funcall fn stream eof-error-p eof-value)
            (error (format nil "type ~a is not defined yet." type))))))

;;; `defbin' helper functions
(defun ensure-named-plist (slot properties)
  "Ensure the named plist for `slot' with default `properties'.
Return (slot-name . plist-properties).
+ `slot' is a symbol or a list with form `(slot-name . plist-properties)';
+ `properties' is a plist standing for default property list."
  (if (listp slot)
      (loop with plist = (cdr slot)
            for (key default-value) on properties by #'cddr
            if (not (getf plist key nil))
              collect (list key default-value) into more
            finally (return (append slot (apply #'append more))))
      (ensure-named-plist (list slot) properties)))

(defmacro with-slots-definitions ((slot-definition slot-defintions
                                   &rest default-plists)
                                  &body body)
  "Process with named plist `slot-definition'.
+ `slot-definition' stands for each named plist in `slot-defintions',
  it's plist would be ensured with `default-plists';
+ `body' stands for corressponding processing code, the code splited
  by tag `:no-use' will be marked as processing for no-use slot.

  A no-use slot is a slot start with `:no-use'."
  (declare (type symbol slot-definition))
  (with-gensyms (slot default-plist-list)
    (loop with no-use-p = nil
          for expr in body
          if (eq expr :no-use)
            do (setf no-use-p t)
          if no-use-p
            collect expr into no-use-expr
          else
            collect expr into norm-expr
          finally (return
                    `(loop with ,default-plist-list = (list ,@default-plists)
                           for ,slot in ,slot-defintions
                           for ,slot-definition
                             = (ensure-named-plist ,slot ,default-plist-list)
                           if (not (eq :no-use (first ,slot-definition)))
                             collect (progn ,@norm-expr)
                           ,@(when no-use-expr
                               `(else collect (progn ,@no-use-expr))))))))

;;; defbin
(defmacro defbin (name (&key (default-type :float) (size-check nil size-check-set-p)
                          (reader-table '*corsika-binary-readers*)
                          (eof-error-p t) eof-value)
                  &body slot-definitions)
  "Define a binary struct and regist corresponding type reader.
+ `name' is the binary struct type name, it would regist a type reader
  with `name' to read (read-type `name' stream);
+ `slot-definitions' should be
   slot-name | (slot-name . slot-property).
+ `default-type' ensures the default type of struct slot;
+ `reader-type' is a variable name for reader hash table;
+ `eof-error-p' and `eof-value' stands for default reader function args."
  (let ((constructor (intern (format nil "MK-~:@(~a~)" name))))
    `(progn
       ;; define binary structure
       (defstruct (,name (:constructor ,constructor)
                         (:conc-name ,(make-symbol (format nil "~:@(~a~)-" name))))
         ,@(with-slots-definitions (slot-def slot-definitions :type default-type)
             (list (car slot-def)
                   (default-type-value (getf (cdr slot-def) :type))
                   :type (default-type-name (getf (cdr slot-def) :type)))))
       
       ;; define binary reader
       (setf (gethash ,(make-keyword name) ,reader-table)
             ,(with-gensyms (stream res)
                `(lambda (,stream &optional (eof-error-p ,eof-error-p)
                                    (eof-value ,eof-value))
                   (let ((,res (,constructor)))
                     ,@(with-slots-definitions (slot-def slot-definitions
                                                         :type default-type
                                                         :offset 1)
                         `(setf (,(intern (format nil "~:@(~a~)-~:@(~a~)"
                                                  name (car slot-def)))
                                 ,res)
                                (read-type ',(getf (cdr slot-def) :type)
                                           ,stream eof-error-p eof-value))
                         :no-use
                         `(dotimes (_ ,(getf (cdr slot-def) :offset))
                            (read-byte ,stream eof-error-p eof-value)))
                     ,res))))

       ;; calculate and regist binary size
       (type-size ',name
                  (+ ,@(with-slots-definitions (slot-def slot-definitions
                                                         :type default-type
                                                         :offset 1)
                         `(type-size ',(getf (cdr slot-def) :type))
                         :no-use
                         (getf (cdr slot-def) :offset))))
       ,(when size-check-set-p
          `(when (not (eq (type-size ',name) ,size-check))
             (error (format nil "Type ~a should be of size ~a rather than ~a."
                            ',name ,size-check (type-size ',name))))))))
