(cl:pushnew :bodge-gl2 cl:*features*)

(asdf:defsystem :society-is-hard
  :description "Society Is Hard Game"
  :author "Alexandru Palade"
  :license "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (trivial-gamekit)
  :pathname "src/"
  :components ((:file "package")
               (:file "util")
               (:file "math")
               (:file "grid")
               (:file "person")
               (:file "killer")
               (:file "police")
               (:file "medic")
               (:file "main")))
