(in-package :cl-corsika/input)

;; The database of the CORSIKA input commands

(defparameter *command-table* (make-hash-table :test 'equal)
  "The Corsika input command table.

Within the *command-table*:
+ KEY: the Corsika name of the command, for example \"RUNNR\";
+ VALUE: the cl-corsika/input infomation plist
  + `:name': the cl-corsika/input command name
  + `:args': the cl-corsika/input command arguments")

(defun search-command (name)
  "Find the Corsika Command by `name'.

Example:

    (search-command \"RUNNR\") ;; => (:NAME RUN-NUMBER :ARGS (ID))
"
  (gethash name *command-table* nil))

;; Defines the input

(defmacro def-input (name lambda-list &rest options)
  "Define the input of the Corsika.
see `corsika-inputs.lisp' for detailed usage. "
  (let* ((corsika   (if (listp name) (second name) name))
         (name      (if (listp name) (first  name) name))
         (docstring (or (second (assoc :documentation options))
                        (format nil "Set input parameter for `~@(~a~)'. " corsika)))
         (args      (multiple-value-bind (arg opt-arg)
                        (parse-ordinary-lambda-list lambda-list)
                      (nconc arg (mapcar #'first opt-arg))))
         (formats   (rest (assoc :formats options)))
         (asserts   (rest (assoc :asserts options)))
         (after     (rest (assoc :after   options))))
    (unless formats (warn (format nil "Missing type hint for ~A. "   name)))
    (unless asserts (warn (format nil "Missing input check for ~A. " name)))
    (flet ((typer (var)   ; turn lambda-var => type (if formatted)
             (case (first (find-if (lambda (type) (find var (rest type))) formats))
               (integer   "~d")
               (float     "~f")
               (boolean   "~:[F~;T~]")
               (otherwise "~a"))))
      `(progn
         ;; define the cl-corsika/input command function
         (defun ,name ,lambda-list
           ,docstring
           ,@(if (eq (first formats) nil) nil
                 `((declare ,@formats)))
           (assert ,@(or asserts (list t)))
           (format *standard-output*
                   ;; ~&CORSIKA ~A ~D ~F ...
                   ,(format nil "~~&~@:(~a~) ~{~a~^ ~}"
                            corsika
                            (mapcar #'typer args))
                   ,@args)
           ,@after)
         ;; register the function in `*command-table*'
         (setf (gethash ,corsika *command-table*)
               '(:name ,name :args ,args))
         ;; return the name of the corsika input
         ',name))))
