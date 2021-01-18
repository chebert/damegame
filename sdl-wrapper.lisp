(in-package #:sdl-wrapper)

(define-alien-routine ("NextEvent" next-event%!) int
  (scancode int :out)
  (mouse-button int :out)
  (clicks int :out)
  (mouse-x int :out)
  (mouse-y int :out)
  (text (array char 32) :out))

(defstruct event-quit)
(defstruct event-keydown
  scancode)
(defstruct event-keyup
  scancode)
(defstruct event-textinput
  text)
(defstruct event-mousedown
  button
  clicks)
(defstruct event-mouseup
  button
  clicks)
(defstruct event-mousewheel
  horizontal-scroll
  vertical-scroll)
(defstruct event-mousemove
  x y)

(defun mouse-button (sdl-mouse-button)
  (case sdl-mouse-button
    (1 :left)
    (2 :middle)
    (3 :right)))

(defun next-event! ()
  (multiple-value-bind (type scancode button clicks mouse-x mouse-y text)
      (next-event%!)
    (ecase type
      ;; no-event
      (0 nil)
      (1 (make-event-quit))
      (2 (make-event-keydown :scancode scancode))
      (3 (make-event-keyup :scancode scancode))
      (4 (make-event-textinput :text (cast text c-string)))
      (5 (make-event-mousedown :button (mouse-button button) :clicks clicks))
      (6 (make-event-mouseup :button (mouse-button button) :clicks clicks))
      (7 (make-event-mousewheel :horizontal-scroll mouse-x :vertical-scroll mouse-y))
      (8 (make-event-mousemove :x mouse-x :y mouse-y)))))

(define-alien-routine ("Delay" delay!) void
  (milliseconds int))

(define-alien-routine ("Start" start%!) int
  (width int)
  (height int)
  (audio-frequency int)
  (audio-channels (unsigned 8)))
(define-alien-routine ("Quit" quit!) void)
(defun start! (width height audio-frequency audio-channels)
  (start%! 0 0 audio-frequency audio-channels)
  (quit!)
  (start%! width height audio-frequency audio-channels))

(define-alien-routine ("CreatePixelBufferTexture" create-pixel-buffer-texture!) system-area-pointer
  (width int)
  (height int))
(define-alien-routine ("ReplacePixelBuffer" replace-pixel-buffer!) void
  (texture system-area-pointer)
  (rgba-pixels (* (unsigned 8)))
  (texture-height int))
(define-alien-routine ("LoadBMP" load-bmp!) system-area-pointer
  (path c-string))
(define-alien-routine ("LoadBMPWithColorKey" load-bmp-with-color-key!) system-area-pointer
  (path c-string)
  (r (unsigned 8))
  (g (unsigned 8))
  (b (unsigned 8)))
(define-alien-routine ("FreeTexture" free-texture!) void
  (texture system-area-pointer))

(define-alien-routine ("OpenFont" open-font!) system-area-pointer
  (path c-string)
  (point-size int))
(define-alien-routine ("CloseFont" close-font!) void
  (font system-area-pointer))

(define-alien-routine ("CreateTextTexture" create-text-texture!) system-area-pointer
  (font system-area-pointer)
  (text c-string))

(define-alien-routine ("TextureWidth" texture-width) int
  (texture system-area-pointer))
(define-alien-routine ("TextureHeight" texture-height) int
  (texture system-area-pointer))

(define-alien-routine ("BufferedAudioBytes" buffered-audio-bytes!) (unsigned 32))
(define-alien-routine ("BufferAudio" buffer-audio!) int
  (bytes (* (unsigned 8)))
  (num-bytes (unsigned 32)))
(define-alien-routine ("PauseAudio" pause-audio!) void)
(define-alien-routine ("PlayAudio" play-audio!) void)

(define-alien-routine ("Clear" clear!) void)
(define-alien-routine ("SetDrawColor" set-draw-color!) void
  (r (unsigned 8))
  (g (unsigned 8))
  (b (unsigned 8))
  (a (unsigned 8)))
(define-alien-routine ("DrawRect" draw-rect!) void
  (x (signed 32))
  (y (signed 32))
  (w (signed 32))
  (h (signed 32)))
(define-alien-routine ("FillRect" fill-rect!) void
  (x (signed 32))
  (y (signed 32))
  (w (signed 32))
  (h (signed 32)))
(define-alien-routine ("DrawTexture" draw-texture!) void
  (texture system-area-pointer)
  (sx (signed 32))
  (sy (signed 32))
  (sw (signed 32))
  (sh (signed 32))
  (dx (signed 32))
  (dy (signed 32))
  (dw (signed 32))
  (dh (signed 32)))
(define-alien-routine ("TextureColorMod" texture-color-mod!) void
  (texture system-area-pointer)
  (r (unsigned 8))
  (g (unsigned 8))
  (b (unsigned 8)))
(define-alien-routine ("Present" present!) void)

(defun recompile-sdl-wrapper-dll! ()
  (unload-shared-object (merge-pathnames *default-pathname-defaults* "sdl_wrapper.dll"))
  (let* ((success t)
	 (error-string
	   (with-output-to-string (s)
	     (handler-case (uiop:run-program "sh ./make_dll.sh" :output s :error-output s)
	       (error () (setq success nil))))))
    (when (not success)
      (error "Could not make dll~%~S" error-string)))
  (load-shared-object (merge-pathnames *default-pathname-defaults* "sdl_wrapper.dll")))


(defparameter *audio-frequency* 48000)
(defparameter *audio-channels* 2)

(defun sdl-test! ()
  (start! 800 600 *audio-frequency* *audio-channels*)
  (set-draw-color! 255 0 255 255)
  (clear!)
  (present!)
  (loop for i below 5000
	do (loop while (let ((event (next-event!)))
			 (when event (print event))
			 event))
	   (delay! 1))
  (quit!))
