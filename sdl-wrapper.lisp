(in-package #:sdl-wrapper)

(define-alien-routine ("NextEvent" next-event%!) int
  (scancode int :out)
  (mouse-button int :out)
  (clicks int :out)
  (mouse-x int :out)
  (mouse-y int :out)
  (text (* char)))

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

(defparameter *text-buffer* (make-alien char 32))
(defun next-event! ()
  "Retrieves the next event from the queue or NIL if there are no more events."
  (multiple-value-bind (type scancode button clicks mouse-x mouse-y)
      (next-event%! *text-buffer*)
    (ecase type
      ;; no-event
      (0 nil)
      (1 (make-event-quit))
      (2 (make-event-keydown :scancode scancode))
      (3 (make-event-keyup :scancode scancode))
      (4 (make-event-textinput :text (cast *text-buffer* c-string)))
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
(define-alien-routine ("ReplacePixelBuffer" replace-pixel-buffer%!) boolean
  "Replaces the pixel buffer of the texture created with create-pixel-buffer-texture!.
Format of each pixel is 4 8-bit unsigned integers R,G,B,A, organized from left-to-right top-to-bottom."
  (texture system-area-pointer)
  (rgba-pixels (* (unsigned 8))))

(defun replace-pixel-buffer! (texture rgba-pixels)
  (sb-sys:with-pinned-objects (rgba-pixels)
    (replace-pixel-buffer%! texture (sb-sys:vector-sap rgba-pixels))))

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
(define-alien-routine ("ElapsedMilliseconds" elapsed-milliseconds) (unsigned 32)
  "Returns the time elapsed since start was called in milliseconds.")

(defparameter *pixel-array*
  (let* ((w 200)
	 (h 300)
	 (pixel-array (make-array (* 4 w h) :element-type '(unsigned-byte 8))))
    (loop for x below w do
      (loop for y below h do
	(let ((idx (* 4 (+ x (* y w)))))
	  (setf (aref pixel-array (+ 0 idx)) (truncate (* (/ x w) 255)))
	  (setf (aref pixel-array (+ 1 idx)) 0)
	  (setf (aref pixel-array (+ 2 idx)) (truncate (* (/ y h) 255)))
	  (setf (aref pixel-array (+ 3 idx)) 0))))
    pixel-array))

(defun sdl-test! ()
  (let ((width 800)
	(height 600)
	(audio-frequency 48000)
	(audio-channels 2))
    (start! width height audio-frequency audio-channels))
  (set-draw-color! 255 0 255 255)
  (clear!)
  (let* ((texture (create-pixel-buffer-texture! 200 300)))
    (replace-pixel-buffer! texture *pixel-array*)
    (draw-texture! texture
		   0 0 (texture-width texture) (texture-height texture)
		   32 32 (texture-width texture) (texture-height texture))
    (free-texture! texture))
  (let* ((font (open-font! "DroidSansMono.ttf" 16))
	 (texture (create-text-texture! font "I want to live.")))
    (texture-color-mod! texture 0 255 0)
    (draw-texture! texture
		   0 0 (texture-width texture) (texture-height texture)
		   300 32 (texture-width texture) (texture-height texture))
    (free-texture! texture)
    (close-font! font))

  (set-draw-color! 0 255 0 255)
  (draw-rect! 400 100 40 80)
  (fill-rect! 400 200 40 80)
  (present!)
  (let ((quit? nil))
    (loop until quit?
	  do (loop while (let ((event (next-event!)))
			   (when (event-quit-p event)
			     (setq quit? t))
			   event))
	     (delay! 1)))
  (quit!))

(defun concat (strings)
  (apply 'concatenate 'string strings))

(defun c-typedef (old-c-type new-c-type)
  (format nil "typedef ~A ~A" old-c-type new-c-type))

(defun c-statement (str)
  (format nil "~A;" str))

(defun lines (strs)
  (concat (mapcar (lambda (str) (format nil "~A~%" str)) strs)))
(defun c-statements (strs)
  (lines (mapcar 'c-statement strs)))

(defun key-c-type (key)
  (ecase key
    (:u8 "u8")
    (:u16 "u16")
    (:u32 "u32")
    (:u64 "u64")
    (:s8 "s8")
    (:s16 "s16")
    (:s32 "s32")
    (:s64 "s64")
    (:int "int")
    (:void "void")
    (:char* "char*")
    (:u8* "u8*")

    (:texture "SDL_Texture*")
    (:font "TTF_Font*")))

(defun c-type (type)
  (if (keywordp type)
      (key-c-type type)
      type))

(defun c-include-relative (relative-path)
  (format nil "#include \"~A\"" relative-path))
(defun c-include-system (system-path)
  (format nil "#include <~A>" system-path))
(defun c-constant (type c-name c-value)
  (format nil "const ~A ~A = ~A" (c-type type) c-name c-value))
(defun c-global (type c-name &optional c-value)
  (if c-value
      (format nil "static ~A ~A = ~A" (c-type type) c-name c-value)
      (format nil "static ~A ~A" (c-type type) c-name)))

(defun c-enum (c-enum-name c-enum-fields)
  (with-output-to-string (s)
    (format s "enum ~A {~%" c-enum-name)
    (loop for field in c-enum-fields
	  for i from 0 do
	    (if (= i (1- (length c-enum-fields)))
		(format s "  ~A~%" field)
		(format s "  ~A,~%" field)))
    (format s "}")))

(defun c-parameter (c-parameter)
  (let* ((type (car c-parameter))
	 (c-name (cdr c-parameter)))
    (format nil "~A ~A" (c-type type) c-name)))

(defun c-function-declaration (type c-name c-parameters &optional c-specifiers)
  (with-output-to-string (s)
    (loop for c-specifier in c-specifiers do (format s "~A " c-specifier))
    (format s "~A ~A(" (c-type type) c-name)
    (loop for c-parameter in c-parameters
	  for i from 0 do
	    (if (= i (1- (length c-parameters)))
		(format s "~A" (c-parameter c-parameter))
		(format s "~A, " (c-parameter c-parameter))))
    (format s ")")))

(defun indent (c-code)
  (with-output-to-string (s)
    (let* ((lines (uiop:split-string c-code :separator '(#\newline))))
      (loop for line in lines
	    for i from 0 do
	      (if (= i (1- (length lines)))
		  (format s "  ~A" line)
		  (format s "  ~A~%" line))))))

(defun c-function-definition (type c-name c-parameters c-body &optional c-specifiers)
  (with-output-to-string (s)
    (format s "~A {~%" (c-function-declaration type c-name c-parameters c-specifiers))
    (format s "~A" (indent c-body))
    (format s "~%}")))

(defun c-function (type c-name c-parameters c-body &optional c-specifiers)
  (list (cons :type type)
	(cons :c-name c-name)
	(cons :c-parameters c-parameters)
	(cons :c-body c-body)
	(cons :c-specifiers c-specifiers)))
(defun c-exported-function (type c-name c-parameters c-body)
  (list (cons :type type)
	(cons :c-name c-name)
	(cons :c-parameters c-parameters)
	(cons :c-body c-body)
	(cons :c-specifiers '("DLL_EXPORT"))))

(defun alist (&rest keys-and-values)
  (labels ((rec (keys-and-values result)
	     (if keys-and-values
		 (let* ((key (first keys-and-values))
			(value (second keys-and-values))
			(rest (rest (rest keys-and-values))))
		   (rec rest (acons key value result)))
		 (nreverse result))))
    (rec keys-and-values ())))

(defun sdl-wrapper-c-functions ()
  (list
   (c-exported-function
    :int "Start" (alist :int "window_width"
			:int "window_height"
			:int "audio_frequency"
			:u8 "audio_channels")
    "#define CHECK(expr) do { int error = expr; if (error) return error; } while(0);
CHECK(SDL_Init(SDL_INIT_EVERYTHING));
CHECK(TTF_Init());
CHECK(SDL_CreateWindowAndRenderer(window_width, window_height, 0, &g_window, &g_renderer));
SDL_StartTextInput();

SDL_AudioSpec desired;
SDL_zero(desired);
desired.freq = g_audio_frequency = audio_frequency;
desired.format = AUDIO_FORMAT;
desired.channels = g_audio_channels = audio_channels;
desired.samples = AUDIO_FRAMES;
desired.callback = NULL;
g_audio_device_id = SDL_OpenAudioDevice(NULL, 0, &desired, &g_obtained_audio_spec, 0);
if (!g_audio_device_id)
  return -1;
#undef CHECK
return 0;")

   (c-exported-function
    :void "Quit" ()
    "SDL_DestroyRenderer(g_renderer);
SDL_DestroyWindow(g_window);
SDL_CloseAudioDevice(g_audio_device_id);
TTF_Quit();
SDL_Quit();")

   (c-exported-function
    :char* "ErrorString" ()
    "return SDL_GetError();")

   (c-exported-function
    :texture "CreatePixelBufferTexture"
    (alist :int "width" :int "height")
    "return SDL_CreateTexture(g_renderer,
    SDL_PIXELFORMAT_ABGR8888,
    SDL_TEXTUREACCESS_STREAMING, 
    width, height);")

   (c-exported-function
    :int "ReplacePixelBuffer"
    (alist :texture "texture"
	   :u8* "rgba_pixels")
    "void *pixels;
int *pitch;
int width, height;
if (SDL_LockTexture(texture, NULL, &pixels, &pitch))
return 0;

SDL_QueryTexture(texture, 0, 0, &width, &height);

memcpy(pixels, rgba_pixels, width * height * 4);
SDL_UnlockTexture(texture);
return 1;")

   (c-exported-function
    :texture "LoadBMP"
    (alist :char* "path")
    "SDL_Surface *surface = SDL_LoadBMP(path);
if (!surface) return NULL;
SDL_Texture *texture = SDL_CreateTextureFromSurface(g_renderer, surface);
SDL_FreeSurface(surface);
return texture;")

   (c-exported-function
    :texture "LoadBMPWithColorKey" (alist :char* "path"
					  :u8 "r"
					  :u8 "g"
					  :u8 "b")
    "SDL_Surface *surface = SDL_LoadBMP(path);
if (!surface) return NULL;
// Enable color key.
SDL_SetColorKey(surface, SDL_TRUE, SDL_MapRGB(surface->format, r, g, b));
SDL_Texture *texture = SDL_CreateTextureFromSurface(g_renderer, surface);
SDL_FreeSurface(surface);
return texture;")

   (c-exported-function
    :texture "FreeTexture"
    (alist :texture "texture")
    "SDL_DestroyTexture(texture);")


   (c-exported-function
    :int "TextureWidth"
    (alist :texture "texture")
    "int width;
SDL_QueryTexture(texture, 0, 0, &width, 0);
return width;")

   (c-exported-function
    :int "TextureHeight"
    (alist :texture "texture")
    "int height;
SDL_QueryTexture(texture, 0, 0, 0, &height);
return height;")


   (c-exported-function
    :font "OpenFont" (alist :char* "path" :int "point_size")
    "return TTF_OpenFont(path, point_size);")
   (c-exported-function
    :void "CloseFont" (alist :font "font")
    "TTF_CloseFont(font);")

   (c-exported-function
    :texture "CreateTextTexture" (alist :font "font" :char* "text")
    "SDL_Color color = {.r = 255, .g = 255, .b = 255};
SDL_Surface *surface = TTF_RenderUTF8_Solid(font, text, color);
if (!surface) return NULL;
SDL_Texture *texture = SDL_CreateTextureFromSurface(g_renderer, surface);
SDL_FreeSurface(surface);
return texture;")


   (c-exported-function
    :void "Clear" ()
    "SDL_RenderClear(g_renderer);")
   (c-exported-function
    :void "SetDrawColor" (alist :u8 "r" :u8 "g" :u8 "b" :u8 "a")
    "SDL_SetRenderDrawColor(g_renderer, r, g, b, a);")
   (c-exported-function
    :void "DrawRect" (alist :s32 "x" :s32 "y" :s32 "w" :s32 "h")
    "SDL_Rect rect = { .x=x, .y=y, .w=w, .h=h };
SDL_RenderDrawRect(g_renderer, &rect);")
   (c-exported-function
    :void "FillRect" (alist :s32 "x" :s32 "y" :s32 "w" :s32 "h")
    "SDL_Rect rect = { .x=x, .y=y, .w=w, .h=h };
SDL_RenderFillRect(g_renderer, &rect);")
   (c-exported-function
    :void "DrawTexture" (alist :texture "texture"
			       :int "sx" :int "sy" :int "sw" :int "sh"
			       :int "dx" :int "dy" :int "dw" :int "dh")
    "SDL_Rect src = { .x=sx, .y=sy, .w=sw, .h=sh };
SDL_Rect dest = { .x=dx, .y=dy, .w=dw, .h=dh };
SDL_RenderCopy(g_renderer, texture, &src, &dest);")

   (c-exported-function
    :void "TextureColorMod" (alist :texture "texture"
				   :u8 "r"
				   :u8 "g"
				   :u8 "b")
    "SDL_SetTextureColorMod(texture, r, g, b);")
   (c-exported-function
    :void "Present" ()
    "SDL_RenderPresent(g_renderer);")

   (c-exported-function
    :u32 "BufferedAudioBytes" ()
    "return SDL_GetQueuedAudioSize(g_audio_device_id);")
   (c-exported-function
    :int "BufferAudio" (alist :u8* "bytes" :u32 "num_bytes")
    "return SDL_QueueAudio(g_audio_device_id, bytes, num_bytes);")
   (c-exported-function
    :void "PauseAudio" ()
    "SDL_PauseAudioDevice(g_audio_device_id, 1);")
   (c-exported-function
    :void "PlayAudio" ()
    "SDL_PauseAudioDevice(g_audio_device_id, 0);")
   (c-exported-function
    :void "Delay" (alist :int "milliseconds")
    "SDL_Delay(milliseconds);")

   (c-exported-function
    "enum EventType" "NextEvent" (alist "SDL_Scancode*" "scancode"
					"int*" "mouse_button"
					"int*" "clicks"
					"int*" "mouse_x"
					"int*" "mouse_y"
					:char* "text")
    "SDL_Event event;
while (SDL_PollEvent(&event)) {
  switch (event.type) {
    case SDL_QUIT:
      return QUIT;
    case SDL_KEYDOWN:
      if (!event.key.repeat) {
        *scancode = event.key.keysym.scancode;
        return KEYDOWN;
      }
      break;
    case SDL_KEYUP:
      *scancode = event.key.keysym.scancode;
      return KEYUP;
    case  SDL_TEXTINPUT:
      memcpy(text, event.text.text, sizeof(event.text.text));
      return TEXTINPUT;
    case SDL_MOUSEMOTION:
      *mouse_x = event.motion.x;
      *mouse_y = event.motion.y;
      return MOUSEMOVE;
    case SDL_MOUSEBUTTONDOWN:
      *mouse_x = event.button.x;
      *mouse_y = event.button.y;
      *mouse_button = event.button.button;
      *clicks = event.button.clicks;
      return MOUSEDOWN;
    case SDL_MOUSEBUTTONUP:
      *mouse_x = event.button.x;
      *mouse_y = event.button.y;
      *mouse_button = event.button.button;
      *clicks = event.button.clicks;
      return MOUSEUP;
    case  SDL_MOUSEWHEEL:
      *mouse_x = event.wheel.x;
      *mouse_y = event.wheel.y;
      return MOUSEWHEEL;
  }
}
return NO_EVENT;")

   (c-exported-function
    :char* "ScancodeName" (alist "SDL_Scancode" "scancode")
    "return SDL_GetScancodeName(scancode);")

   (c-exported-function
    "SDL_Scancode" "GetScancodeFromName" (alist :char* "name")
    "return SDL_GetScancodeFromName(name);")

   (c-exported-function
    :u32 "ElapsedMilliseconds" ()
    "return SDL_GetTicks();")))

;; Idea: writing C code in Lisp
(defun sdl-wrapper-c-code ()
  "The C Code for the SDL Wrapper."
  (concat
   (list
    (lines
     (list
      (c-include-relative "SDL.h")
      (c-include-relative "SDL_ttf.h")
      (c-include-system "stdint.h")
      "#define DLL_EXPORT __declspec(dllexport)"))

    ;; Typedefs
    (c-statements
     (mapcar
      (lambda (old-type new-type-key) (c-typedef old-type (key-c-type new-type-key)))
      '("uint8_t" "uint16_t" "uint32_t" "uint64_t" "int8_t" "int16_t" "int32_t" "int64_t")
      '(:u8 :u16 :u32 :u64 :s8 :s16 :s32 :s64)))

    ;; Parameters
    (c-statements
     (list
      (c-constant "SDL_AudioFormat" "AUDIO_FORMAT" "AUDIO_S16LSB")
      ;; An audio frame is a collection of samples that make up a channel.
      (c-constant :u16 "AUDIO_FRAMES" 4096)
      (c-global :int "g_audio_frequency" 48000)
      (c-global :u8 "g_audio_channels" 2)

      ;; Globals
      (c-global "SDL_Window*" "g_window")
      (c-global "SDL_Renderer*" "g_renderer")
      (c-global "SDL_AudioDeviceID" "g_audio_device_id")
      (c-global "SDL_AudioSpec" "g_obtained_audio_spec")

      ;; Data Structures
      (c-enum
       "EventType"
       (list
	"NO_EVENT"
	"QUIT"
	"KEYDOWN"
	"KEYUP"
	"TEXTINPUT"
	"MOUSEDOWN"
	"MOUSEUP"
	"MOUSEWHEEL"
	"MOUSEMOVE"))))

    (c-statements
     (mapcar (lambda (c-function)
	       (c-function-declaration
		(cdr (assoc :type c-function))
		(cdr (assoc :c-name c-function))
		(cdr (assoc :c-parameters c-function))
		(cdr (assoc :c-specifiers c-function))))
	     (sdl-wrapper-c-functions)))

    (lines
     (mapcar (lambda (c-function)
	       (c-function-definition
		(cdr (assoc :type c-function))
		(cdr (assoc :c-name c-function))
		(cdr (assoc :c-parameters c-function))
		(cdr (assoc :c-body c-function))
		(cdr (assoc :c-specifiers c-function))))
	     (sdl-wrapper-c-functions))))))

(defparameter *relative-include-dirs*
  (list "SDL2_x64/include/SDL2"
	"SDL2_ttf_x64/include/SDL2"))
(defparameter *relative-lib-dirs*
  (list "SDL2_x64/lib"
	"SDL2_ttf_x64/lib"))
(defun include-flags ()
  (with-output-to-string (s)
    (mapcar (lambda (path) (format s "-I./~A " path))
	    *relative-include-dirs*)))
(defun lib-flags ()
  (with-output-to-string (s)
    (mapcar (lambda (path) (format s "-L./~A " path))
	    *relative-lib-dirs*)))
(defun compile-flags ()
  "-w -Wl,-subsystem,windows")
(defun link-flags ()
  "-lmingw32 -lSDL2main -lSDL2 -lSDL2_ttf")
(defparameter *c-filename* "sdl_wrapper.c")
(defparameter *dll-filename* "sdl_wrapper.dll")

(defun build-dll-command ()
  (format nil "gcc -shared -o ~A ~A ~A ~A ~A ~A"
	  *dll-filename*
	  *c-filename*
	  (include-flags)
	  (lib-flags)
	  (compile-flags)
	  (link-flags)))

(defun build-dll! (&key (output-stream *standard-output*)
		     (error-output-stream *standard-output*))
  (uiop:run-program (build-dll-command)
		    :output output-stream
		    :error-output-stream error-output-stream))

(defun assemble-sdl-wrapper-c-code! ()
  (with-open-file (stream *c-filename* :direction :output :if-exists :supersede)
    (format stream "~A" (sdl-wrapper-c-code))))

(defun recompile-sdl-wrapper-dll! ()
  "Unloads sdl_wrapper.dll, reassembles the c-code, recompiles it, and reloads the resulting dll."
  (let* ((dll-fullname (merge-pathnames *default-pathname-defaults* *dll-filename*)))
    (unload-shared-object dll-fullname)
    ;;(assemble-sdl-wrapper-c-code!)
    (let* ((success t)
	   (error-string
	     (with-output-to-string (s)
	       (handler-case (build-dll! :output-stream s :error-output-stream s)
		 (error () (setq success nil))))))
      (unless success
	(error "Could not make dll~%~S" error-string)))
    (load-shared-object dll-fullname)))
