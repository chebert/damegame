;;;; package.lisp

(DEFPACKAGE #:EXAMPLE
  (:DOCUMENTATION "Provides tools for adding examples/tests.")
  (:USE #:SCHEMEISH.SCHEMEISH)
  (:EXPORT #:CHECK-ALL-EXAMPLES
           #:CHECK-EXAMPLES
           #:DEFINE-EXAMPLES
           #:UNDEFINE-EXAMPLES))

(DEFPACKAGE #:GEOMETRY
  (:DOCUMENTATION "Provides tools for working with geometric figures: 2d-vectors, points, offsets, axis-aligned rectangles.")
  (:USE #:EXAMPLE #:SCHEMEISH.SCHEMEISH)
  (:EXPORT #:IN-RANGE?
           #:MAKE-OFFSET
           #:MAKE-POINT
           #:MAKE-RECT
           #:MAKE-V2
           #:OFFSET
           #:OFFSET-X
           #:OFFSET-Y
           #:OFFSET?
           #:POINT
           #:POINT-IN-RECT?
           #:POINT-X
           #:POINT-Y
           #:POINT?
           #:RECT
           #:RECT-HEIGHT
           #:RECT-WIDTH
           #:RECT-X
           #:RECT-Y
           #:RECT?
           #:V2
           #:V2+
           #:V2-
           #:V2-X
           #:V2-Y
           #:V2-ZERO
           #:V2?))

(DEFPACKAGE #:BINARY
  (:DOCUMENTATION "Provides functions for working with binary numbers including:
 - constructing/deconstructing unsigned and signed 8-bit and 16-bit integers
 - Decimal-adjusted arithmetic operations
 - Rotate and shift operations
 - Setting/resetting individual bits.")
  (:USE #:EXAMPLE #:SCHEMEISH.SCHEMEISH)
  (:EXPORT #:BIN8-STRING
           #:BIT-BORROW-WITH-CARRY?
           #:BIT-BORROW?
           #:BIT-CARRY-WITH-CARRY?
           #:BIT-CARRY?
           #:BIT-EXTRACT
           #:BIT-MASK
           #:BIT-MASK-MATCHES?
           #:BIT-RESET
           #:BIT-RESET?
           #:BIT-SET
           #:BIT-SET?
           #:BIT-SHIFT-LEFT
           #:BIT-SHIFT-RIGHT
           #:BIT-VALUE
           #:BIT-VALUE-SET
           #:BORROW8?
           #:BYTE-COMPLEMENT
           #:CARRY16?
           #:CARRY8?
           #:DECIMAL-ADJUSTED-ARITHMETIC
           #:DECIMAL-ADJUSTED-ARITHMETIC-CARRY?
           #:HALF-BORROW8?
           #:HALF-CARRY16?
           #:HALF-CARRY8?
           #:HEX16-STRING
           #:HEX8-STRING
           #:HIGH-BYTE
           #:HIGH-NIBBLE
           #:INVERT-BYTE
           #:LOW-BYTE
           #:LOW-NIBBLE
           #:ROTATE-LEFT
           #:ROTATE-RIGHT
           #:S16-STRING
           #:S8-STRING
           #:SWAP-NIBBLES
           #:U16
           #:U16->S16
           #:U16-STRING
           #:U8
           #:U8->S8
           #:U8-STRING
           #:VALUE->U16))

(DEFPACKAGE #:RAM
  (:USE #:EXAMPLE #:SCHEMEISH.SCHEMEISH)
  (:EXPORT #:INTERNAL-RAM?
           #:MAKE-INTERNAL-RAM
           #:MAKE-MEMORY-VECTOR
           #:WITH-MAPPED-ADDRESS))

(DEFPACKAGE #:OPCODE-COMPILER
  (:USE #:BINARY #:EXAMPLE #:SCHEMEISH.SCHEMEISH)
  (:EXPORT #:COMPILE-EXECUTION-PROC
           #:FIND-COMPILER
           #:FLAG-GETTER
           #:IGNORE-IMMEDIATE16
           #:IGNORE-IMMEDIATE8
           #:IGNORE-IMMEDIATES
           #:JUST-IMMEDIATE16
           #:JUST-IMMEDIATE8
           #:JUST-MACHINE
           #:OPCODE-MATCHES-TEMPLATE?
           #:R-CODE->GETTER
           #:R-CODE->REGISTER-NAME
           #:R-CODE->SETTER
           #:REGISTER-COMPILER!))

(DEFPACKAGE #:FLAGS
  (:USE #:BINARY #:EXAMPLE #:SCHEMEISH.SCHEMEISH)
  (:EXPORT #:CARRY-BIT
           #:CARRY-SET?
           #:FLAG-MASK
           #:FLAGS
           #:FLAGS->LIST
           #:HALF-CARRY-SET?
           #:SUBRACTION-SET?
           #:ZERO-SET?))

(DEFPACKAGE #:MACHINE
  (:USE #:BINARY
        #:EXAMPLE
        #:FLAGS
        #:OPCODE-COMPILER
        #:RAM
        #:SCHEMEISH.SCHEMEISH)
  (:EXPORT #:MACHINE?
           #:MAKE-MACHINE
           #:MAKE-REGISTER
           #:MAKE-ROM
           #:REGISTER-GETTER
           #:REGISTER-SETTER
           #:REGISTER?
           #:ROM?))

(defpackage #:opcode-parser
  (:use #:schemeish.schemeish #:machine #:opcode-compiler))

(DEFPACKAGE #:TRANSFERS
  (:DOCUMENTATION "Installs opcode compilers for 8-bit transfer instructions: LD.")
  (:USE #:BINARY
        #:EXAMPLE
        #:FLAGS
        #:MACHINE
        #:OPCODE-COMPILER
        #:OPCODE-PARSER
        #:SCHEMEISH.SCHEMEISH))

(DEFPACKAGE #:16BIT-ALU
  (:DOCUMENTATION "Installs 16-bit alu opcode compilers: ADD, INC, DEC.")
  (:USE #:BINARY
        #:FLAGS
        #:MACHINE
        #:OPCODE-COMPILER
        #:OPCODE-PARSER
        #:SCHEMEISH.SCHEMEISH))

(DEFPACKAGE #:8BIT-ALU
  (:DOCUMENTATION "Installs opcode compilers for 8-bit arithmetic instructions:
   ADD, ADC, SUB, SBC
8-bit logical instructions:
  AND, OR, XOR, CP
and 8-bit updates:
  INC, DEC")
  (:USE #:BINARY
        #:EXAMPLE
        #:FLAGS
        #:MACHINE
        #:OPCODE-COMPILER
        #:OPCODE-PARSER
        #:SCHEMEISH.SCHEMEISH))

(DEFPACKAGE #:OPCODE-PARSER
  (:USE #:MACHINE #:OPCODE-COMPILER #:SCHEMEISH.SCHEMEISH)
  (:EXPORT #:R-CODE->GETTER #:R-CODE->REGISTER-NAME #:R-CODE->SETTER))


(defpackage #:sdl-wrapper
  (:use #:cl #:sb-alien)
  (:export
   #:present!
   #:texture-color-mod!
   #:draw-texture!
   #:event-quit
   #:event-keydown
   #:event-keydown-scancode
   #:event-keyup-scancode
   #:event-keyup
   #:make-event-quit
   #:event-quit-p
   #:copy-event-quit
   #:make-event-keydown
   #:event-keydown-p
   #:copy-event-keydown
   #:make-event-keyup
   #:event-keyup-p
   #:copy-event-keyup
   #:event-textinput
   #:make-event-textinput
   #:event-textinput-p
   #:copy-event-textinput
   #:event-textinput-text
   #:event-mousedown
   #:make-event-mousedown
   #:event-mousedown-p
   #:copy-event-mousedown
   #:event-mousedown-button
   #:event-mousedown-clicks
   #:event-mouseup
   #:make-event-mouseup
   #:event-mouseup-p
   #:copy-event-mouseup
   #:event-mouseup-button
   #:event-mouseup-clicks
   #:event-mousewheel
   #:make-event-mousewheel
   #:event-mousewheel-p
   #:copy-event-mousewheel
   #:event-mousewheel-horizontal-scroll
   #:event-mousewheel-vertical-scroll
   #:event-mousemove
   #:make-event-mousemove
   #:event-mousemove-p
   #:copy-event-mousemove
   #:event-mousemove-x
   #:event-mousemove-y
   #:next-event!
   #:delay!
   #:quit!
   #:start!
   #:create-pixel-buffer-texture!
   #:replace-pixel-buffer!
   #:load-bmp!
   #:load-bmp-with-color-key!
   #:free-texture!
   #:open-font!
   #:close-font!
   #:create-text-texture!
   #:texture-width
   #:texture-height
   #:buffered-audio-bytes!
   #:buffer-audio!
   #:pause-audio!
   #:play-audio!
   #:clear!
   #:set-draw-color!
   #:draw-rect!
   #:fill-rect!
   #:recompile-sdl-wrapper-dll!
   #:error-string
   #:scancode-name
   #:scancode-from-name
   #:elapsed-milliseconds))

(defpackage #:damegame
  (:shadowing-import-from #:cl #:lambda)
  (:use #:schemeish.schemeish #:sdl-wrapper))
