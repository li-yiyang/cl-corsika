(defpackage #:cl-corsika/physics
  (:use :cl)
  (:export
   ;; particles.lisp
   #:corsika-particle-name
   #:corsika-particle-id

   ;; utils.lisp
   #:muon?
   #:muon-add-info?
   #:gamma?
   #:electron?
   ))

(in-package :cl-corsika/physics)
