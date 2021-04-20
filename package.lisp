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
           #:BIT-RESET
           #:BIT-RESET?
           #:BIT-SET
           #:BIT-SET?
           #:BIT-VALUE
           #:BIT-VALUE-SET
           #:DECIMAL-ADJUSTED-ARITHMETIC
           #:DECIMAL-ADJUSTED-ARITHMETIC-CARRY?
           #:HEX16-STRING
           #:HEX8-STRING
           #:HIGH-BYTE
           #:HIGH-NIBBLE
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
           #:U8-STRING))


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
