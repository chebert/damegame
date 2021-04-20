;;;; damegame.asd

(asdf:defsystem #:damegame
  :description "Describe damegame here"
  :author "Chebert"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("schemeish")
  :components ((:file "package")
	       (:file "example")
	       (:file "binary")
	       (:file "geometry")
	       (:file "sdl-wrapper")
               (:file "damegame")))
