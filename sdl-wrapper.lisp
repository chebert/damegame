(in-package #:sdl-wrapper)

(define-alien-routine ("NextEvent" next-event%!) int
  (scancode int :out)
  (mouse-button int :out)
  (clicks int :out)
  (mouse-x int :out)
  (mouse-y int :out)
  (text (array char 32) :out))

(defstruct event-quit
  "Event that occurs when the user tries to close the window (e.g. Alt-F4 or clicking X in the top right)")
(defstruct event-keydown
  "Event that occurs when a key is pressed. Scancode is the sdl scancode."
  scancode)
(defstruct event-keyup
  "Event that occurs when a key is released. Scancode is the sdl scancode."
  scancode)
(defstruct event-textinput
  "Event that occurs when text has been entered. Text is the string representing the text entered.
(E.g. shift+'x' is the text: 'X'."
  text)
(defstruct event-mousedown
  "Event that occurs when a mouse button is pressed.
Button is the mouse button one of (:left :middle :right)
and clicks is the number of clicks performed (e.g. 2 is a 'double-click)."
  button
  clicks)
(defstruct event-mouseup
  "Event that occurs when a mouse button is released.
Button is the mouse button one of (:left :middle :right)
and clicks is the number of clicks performed (e.g. 2 is a 'double-click)."
  button
  clicks)
(defstruct event-mousewheel
  "Event that occurs when the mouse wheel is scrolled (typically vertically)."
  horizontal-scroll
  vertical-scroll)
(defstruct event-mousemove
  "The event that occurs when the mouse is moved over the window."
  x y)

(defun mouse-button (sdl-mouse-button)
  (case sdl-mouse-button
    (1 :left)
    (2 :middle)
    (3 :right)))

(defun next-event! ()
  "Retrieves the next event from the queue or NIL if there are no more events."
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
  "Pauses the current thread for at least the given duration."
  (milliseconds int))

(define-alien-routine ("Start" start%!) int
  (width int)
  (height int)
  (audio-frequency int)
  (audio-channels (unsigned 8)))
(define-alien-routine ("Quit" quit!) void
  "Quits SDL. Does not destroy any textures or close any fonts (as far as I know).")
(defun start! (width height audio-frequency audio-channels)
  "Starts SDL, setting up SDL_TTF, audio, and creates a single window."
  (start%! 0 0 audio-frequency audio-channels)
  (quit!)
  (start%! width height audio-frequency audio-channels))

(define-alien-routine ("CreatePixelBufferTexture" create-pixel-buffer-texture!) system-area-pointer
  "Creates a texture that can have its pixel buffer replaced using replace-pixel-buffer!"
  (width int)
  (height int))
(define-alien-routine ("ReplacePixelBuffer" replace-pixel-buffer!) void
  "Replaces the pixel buffer of the texture created with create-pixel-buffer-texture!.
Format of each pixel is 4 8-bit unsigned integers R,G,B,A, organized from left-to-right top-to-bottom."
  (texture system-area-pointer)
  (rgba-pixels (* (unsigned 8)))
  (texture-height int))
(define-alien-routine ("LoadBMP" load-bmp!) system-area-pointer
  "Load the BMP."
  (path c-string))
(define-alien-routine ("LoadBMPWithColorKey" load-bmp-with-color-key!) system-area-pointer
  "Load a BMP treating all pixels matching color key as completely transparent."
  (path c-string)
  (r (unsigned 8))
  (g (unsigned 8))
  (b (unsigned 8)))
(define-alien-routine ("FreeTexture" free-texture!) void
  "Releases the texture memory."
  (texture system-area-pointer))

(define-alien-routine ("OpenFont" open-font!) system-area-pointer
  "Opens the font-file at font with the given font-size."
  (path c-string)
  (point-size int))
(define-alien-routine ("CloseFont" close-font!) void
  "Closes the font-file."
  (font system-area-pointer))

(define-alien-routine ("CreateTextTexture" create-text-texture!) system-area-pointer
  "Creates and renders to a texture with the given font and text."
  (font system-area-pointer)
  (text c-string))

(define-alien-routine ("TextureWidth" texture-width) int
  "Returns the width of the SDL_Texture"
  (texture system-area-pointer))
(define-alien-routine ("TextureHeight" texture-height) int
  "Returns the height of the SDL_Texture"
  (texture system-area-pointer))

(define-alien-routine ("BufferedAudioBytes" buffered-audio-bytes!) (unsigned 32)
  "Returns the number of audio bytes currently buffered.")
(define-alien-routine ("BufferAudio" buffer-audio!) int
  "Num-bytes are copied from the bytes array into the audio buffer queue."
  (bytes (* (unsigned 8)))
  (num-bytes (unsigned 32)))
(define-alien-routine ("PauseAudio" pause-audio!) void
  "Pauses audio playback.")
(define-alien-routine ("PlayAudio" play-audio!) void
  "Begins/resumes audio playback of the current audio buffer.")

(define-alien-routine ("Clear" clear!) void
  "Clears the screen to the current draw color.")
(define-alien-routine ("SetDrawColor" set-draw-color!) void
  "Sets the current draw color."
  (r (unsigned 8))
  (g (unsigned 8))
  (b (unsigned 8))
  (a (unsigned 8)))
(define-alien-routine ("DrawRect" draw-rect!) void
  "Draws a rectangular outline with the current draw color."
  (x (signed 32))
  (y (signed 32))
  (w (signed 32))
  (h (signed 32)))
(define-alien-routine ("FillRect" fill-rect!) void
  "Draws a filled rect with the current draw color."
  (x (signed 32))
  (y (signed 32))
  (w (signed 32))
  (h (signed 32)))
(define-alien-routine ("DrawTexture" draw-texture!) void
  "Renders the texture to the screen sx,sy,sw,sh is the rectangular portion of the texture
to render, and dx,dy,dw,dh is the destination rectangle to draw to."
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
  "Modulates the color of the texture with the provided color."
  (texture system-area-pointer)
  (r (unsigned 8))
  (g (unsigned 8))
  (b (unsigned 8)))
(define-alien-routine ("Present" present!) void
  "Flips the display buffer.")

(define-alien-routine ("ErrorString" error-string) void
  "Gets the string representing the last error.")
(define-alien-routine ("ScancodeName" scancode-name) c-string
  "Returns the name of the scancode. Can be called at any time (even before start!)"
  (scancode int))
(define-alien-routine ("ScancodeFromName" scancode-from-name) int
  "Returns the scancode. Can be called at any time (even before start!)."
  (name c-string))


(defun recompile-sdl-wrapper-dll! ()
  "Unloads sdl_wrapper.dll, recompiles it, and reloads it."
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
