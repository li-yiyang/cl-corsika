(defsystem "cl-corsika"
  :author ("凉凉")
  :version "0"
  :description "This is a package that works with Corsika.")

(defsystem "cl-corsika/binary"
  :author ("凉凉")
  :version "0.1"
  :description "This is a package that read Corsika output."
  :depends-on ("alexandria" "ieee-floats")
  :serial t
  :pathname "binary"
  :components ((:file "package")
               (:file "defbin")
               (:file "corsika-defbin")))
