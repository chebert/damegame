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
	       (:file "geometry")
	       (:file "binary")
	       (:file "flags")
	       (:file "opcode-compiler")
	       (:file "ram")
	       (:file "machine")
	       (:file "opcode-parser")
	       (:file "transfers")
	       (:file "8bit-alu")
	       (:file "16bit-alu")
	       (:file "sdl-wrapper")
               (:file "damegame")))
