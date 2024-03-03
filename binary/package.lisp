(defpackage #:cl-corsika/binary
  (:use :cl)
  (:import-from #:ieee-floats
                #:decode-float32)
  (:import-from #:alexandria
                #:make-keyword
                #:with-gensyms)
  (:documentation "This package reads Corsika binary outputs."))

(in-package :cl-corsika/binary)
