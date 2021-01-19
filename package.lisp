;;;; package.lisp

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
  (:use #:cl #:sdl-wrapper))
