(defsystem "cl-corsika"
  :author ("凉凉")
  :version "0"
  :description "This is a package that works with Corsika."
  :depends-on ("cl-corsika/binary")
  :serial t
  :components ((:file "package")))

(defsystem "cl-corsika/binary"
  :author ("凉凉")
  :version "0.1"
  :description "This is a package that read Corsika output."
  :depends-on ("alexandria" "ieee-floats")
  :serial t
  :pathname "binary"
  :components ((:file "package")
               (:file "defbin")
               (:file "corsika-defbin")
               (:file "corsika-reader")
               (:file "higher-level")))

(defsystem "cl-corsika/input"
  :author ("凉凉")
  :version "0.1"
  :description "This is a package that generate Corsika input."
  :depends-on ("alexandria")
  :serial t
  :pathname "input"
  :components ((:file "package")
               (:file "def-input")
               (:file "corsika-inputs")
               (:file "wrapper")))
