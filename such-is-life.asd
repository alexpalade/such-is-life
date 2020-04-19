(cl:pushnew :bodge-gl2 cl:*features*)

(asdf:defsystem :such-is-life
  :description "Such Is Life Game"
  :author "Alexandru Palade"
  :license "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (trivial-gamekit)
  :pathname "src/"
  :components ((:file "package")
               (:file "parameters")
               (:file "sil-game")
               (:file "math")
               (:file "ui/button")
               (:file "ui/adjuster")
               (:file "ui/label")
               (:file "ui/panel")
               (:file "ui/status-bar")
               (:file "ui/separator")
               (:file "grid")
               (:file "util")
               (:file "pathfinding")
               (:file "person")
               (:file "killer")
               (:file "police")
               (:file "medic")
               (:file "main")))
