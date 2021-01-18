;;;; damegame.asd

(asdf:defsystem #:damegame
  :description "Describe damegame here"
  :author "Chebert"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "sdl-wrapper")
               (:file "damegame")))
