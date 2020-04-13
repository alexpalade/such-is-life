(asdf:defsystem :sih
  :description "Society is Hard Game"
  :author "Alexandru Palade"
  :license "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (trivial-gamekit)
  :components ((:file "package")
               (:file "math")
               (:file "grid")
               (:file "actor")
               (:file "policeman")
               (:file "main")))
