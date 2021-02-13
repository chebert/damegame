(in-package #:sdl-wrapper)

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

(defun start! (width height audio-frequency audio-channels)
  "Starts SDL, setting up SDL_TTF, audio, and creates a single window."
  (start%! 0 0 audio-frequency audio-channels)
  (quit!)
  (start%! width height audio-frequency audio-channels))

(defun replace-pixel-buffer! (texture rgba-pixels)
  (sb-sys:with-pinned-objects (rgba-pixels)
    (replace-pixel-buffer%! texture (sb-sys:vector-sap rgba-pixels))))

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
    (:int* "int*")

    (:scancode "SDL_Scancode")
    (:event-type "enum EventType")
    (:texture "SDL_Texture*")
    (:font "TTF_Font*")))
(defun key-alien-type (key)
  (ecase key
    (:u8 '(sb-alien:unsigned 8))
    (:u16 '(sb-alien:unsigned 16))
    (:u32 '(sb-alien:unsigned 32))
    (:u64 '(sb-alien:unsigned 64))
    (:s8 '(sb-alien:signed 8))
    (:s16 '(sb-alien:signed 16))
    (:s32 '(sb-alien:signed 32))
    (:s64 '(sb-alien:signed 64))
    ((:int :scancode :event-type) 'sb-alien:int)
    (:void 'sb-alien:void)
    (:char* 'sb-alien:c-string)
    (:u8* '(sb-alien:* (sb-alien:unsigned 8)))
    (:int* '(sb-alien:* sb-alien:int))

    ((:texture :font) 'sb-alien:system-area-pointer)))

(defun alien-type (type)
  (cond
    ((consp type) (alien-type (cdr type)))
    (t (key-alien-type type))))

(defun c-type (type)
  (cond
    ((keywordp type) (key-c-type type))
    ((consp type)
     (ecase (car type)
       (:out (format nil "~A*" (key-c-type (cdr type))))))
    (t type)))

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

(defun c-parameter (parameter)
  (let* ((type (cdr (assoc :type parameter)))
	 (c-name (cdr (assoc :c-name parameter))))
    (format nil "~A ~A" (c-type type) c-name)))

(defun c-comment (text)
  (with-output-to-string (s)
    (let* ((lines (split-lines text)))
      (loop for line in lines do
	(format s "// ~A~%" line)))))

(defun c-function-declaration (type c-name parameters c-specifiers &optional documentation)
  (with-output-to-string (s)
    (when documentation
      (format s "~A" (c-comment documentation)))
    (loop for c-specifier in c-specifiers do (format s "~A " c-specifier))
    (format s "~A ~A~A" (c-type type) c-name (c-arguments (mapcar 'c-parameter parameters)))))

(defun split-lines (text)
  (uiop:split-string text :separator '(#\newline)))

(defun indent (c-code)
  (with-output-to-string (s)
    (let* ((lines (split-lines c-code)))
      (loop for line in lines
	    for i from 0 do
	      (if (= i (1- (length lines)))
		  (format s "  ~A" line)
		  (format s "  ~A~%" line))))))

(defun c-function-definition (type c-name parameters c-body c-specifiers)
  (with-output-to-string (s)
    (format s "~A {~%" (c-function-declaration type c-name parameters c-specifiers))
    (format s "~A" (indent c-body))
    (format s "~%}")))

(defun c-exported-function (type c-name lisp-name parameters documentation c-body)
  (assert (stringp documentation))
  (assert (listp parameters))
  (assert (stringp c-name))
  (assert (stringp c-body))
  (assert (symbolp lisp-name))
  (list (cons :type type)
	(cons :c-name c-name)
	(cons :lisp-name lisp-name)
	(cons :parameters parameters)
	(cons :c-body c-body)
	(cons :documentation documentation)
	(cons :c-specifiers '("DLL_EXPORT"))))

(defun alien-function-definition (type c-name lisp-name parameters documentation)
  `(define-alien-routine (,c-name ,lisp-name) ,(alien-type type)
     ,documentation
     ,@(mapcar (lambda (parameter)
		 (let* ((name (cdr (assoc :lisp-name parameter)))
			(type (cdr (assoc :type parameter))))
		   (if (and (consp type) (eql :out (car type)))
		       `(,name ,(alien-type type) :out)
		       `(,name ,(alien-type type)))))
	       parameters)))

(defun out-parameter-type (type)
  (cons :out type))

(defun parameter (type c-name lisp-name)
  (assert (symbolp lisp-name))
  (assert (stringp c-name))
  (list (cons :type type)
	(cons :c-name c-name)
	(cons :lisp-name lisp-name)))

(defun parameters (&rest types-and-c-names-and-lisp-names)
  (labels ((rec (types-and-c-names-and-lisp-names result)
	     (if types-and-c-names-and-lisp-names
		 (let* ((type (first types-and-c-names-and-lisp-names))
			(c-name (second types-and-c-names-and-lisp-names))
			(lisp-name (third types-and-c-names-and-lisp-names))
			(rest (rest (rest (rest types-and-c-names-and-lisp-names)))))
		   (rec rest (cons (parameter type c-name lisp-name) result)))
		 (nreverse result))))
    (rec types-and-c-names-and-lisp-names ())))

(defun define-sdl-wrapper-alien-routines! ()
  (eval (cons 'list (mapcar (lambda (spec)
			      (alien-function-definition
			       (cdr (assoc :type spec))
			       (cdr (assoc :c-name spec))
			       (cdr (assoc :lisp-name spec))
			       (cdr (assoc :parameters spec))
			       (cdr (assoc :documentation spec))))
			    (sdl-wrapper-c-functions)))))
(defun comma-separated (strings)
  (with-output-to-string (s)
    (loop for string in strings
	  for i from 0 do
	    (if (= i (1- (length strings)))
		(format s "~A" string)
		(format s "~A, " string)))))
(defun c-arguments (strings)
  (format nil "(~A)" (comma-separated strings)))

(defun sdl-wrapper-c-functions ()
  (list
   (c-exported-function
    :int "Start" 'start%!
    (parameters :int "window_width" 'window-width
		:int "window_height" 'window-height
		:int "audio_frequency" 'audio-frequency
		:u8 "audio_channels" 'audio-channels)
    "Starts SDL. Loads TTF. Creates a window/renderer.
Returns a non-zero error code on failure."
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
    :void "Quit" 'quit! ()
    "Closes the window and renderer. Quits SDL and SDL_TTF."
    "SDL_DestroyRenderer(g_renderer);
SDL_DestroyWindow(g_window);
SDL_CloseAudioDevice(g_audio_device_id);
TTF_Quit();
SDL_Quit();")

   (c-exported-function
    :char* "ErrorString" 'error-string ()
    "Returns the error string for the last SDL error."
    "return SDL_GetError();")

   (c-exported-function
    :texture "CreatePixelBufferTexture" 'create-pixel-buffer-texture!
    (parameters :int "width" 'width
		:int "height" 'height)
    "Creates a texture with a modifiable pixel buffer.
Format of color is 4 u8's: R,G,B,A"
    "return SDL_CreateTexture(g_renderer,
    SDL_PIXELFORMAT_ABGR8888,
    SDL_TEXTUREACCESS_STREAMING, 
    width, height);")

   (c-exported-function
    :int "ReplacePixelBuffer" 'replace-pixel-buffer%!
    (parameters :texture "texture" 'texture
		:u8* "rgba_pixels" 'rgba-pixels)
    "Replaces the pixel buffer of a texture that hs been created with CreatePixelBufferTexture."
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
    :texture "LoadBMP" 'load-bmp!
    (parameters :char* "path" 'path)
    "Loads a BMP from path.
Returns NULL on failure"
    "SDL_Surface *surface = SDL_LoadBMP(path);
if (!surface) return NULL;
SDL_Texture *texture = SDL_CreateTextureFromSurface(g_renderer, surface);
SDL_FreeSurface(surface);
return texture;")

   (c-exported-function
    :texture "LoadBMPWithColorKey" 'load-bmp-with-color-key!
    (parameters :char* "path" 'path
		:u8 "r" 'r
		:u8 "g" 'g
		:u8 "b" 'b)
    "Loads a BMP from path, using pure black as a transparent pixel.
Returns NULL on failure"
    "SDL_Surface *surface = SDL_LoadBMP(path);
if (!surface) return NULL;
// Enable color key.
SDL_SetColorKey(surface, SDL_TRUE, SDL_MapRGB(surface->format, r, g, b));
SDL_Texture *texture = SDL_CreateTextureFromSurface(g_renderer, surface);
SDL_FreeSurface(surface);
return texture;")

   (c-exported-function
    :texture "FreeTexture" 'free-texture!
    (parameters :texture "texture" 'texture)
    "Frees the memory associated with texture."
    "SDL_DestroyTexture(texture);")


   (c-exported-function
    :int "TextureWidth" 'texture-width
    (parameters :texture "texture" 'texture)
    "Return width of texture."
    "int width;
SDL_QueryTexture(texture, 0, 0, &width, 0);
return width;")

   (c-exported-function
    :int "TextureHeight" 'texture-height
    (parameters :texture "texture" 'texture)
    "Return height of texture."
    "int height;
SDL_QueryTexture(texture, 0, 0, 0, &height);
return height;")


   (c-exported-function
    :font "OpenFont" 'open-font!
    (parameters :char* "path" 'path
		:int "point_size" 'point-size)
    "Open the font at path with the given point_size. Return NULL on failure."
    "return TTF_OpenFont(path, point_size);")
   (c-exported-function
    :void "CloseFont" 'close-font!
    (parameters :font "font" 'font)
    "Close the font."
    "TTF_CloseFont(font);")

   (c-exported-function
    :texture "CreateTextTexture" 'create-text-texture!
    (parameters :font "font" 'font
		:char* "text" 'text)
    "Creates white text on a transparent background."
    "SDL_Color color = {.r = 255, .g = 255, .b = 255};
SDL_Surface *surface = TTF_RenderUTF8_Solid(font, text, color);
if (!surface) return NULL;
SDL_Texture *texture = SDL_CreateTextureFromSurface(g_renderer, surface);
SDL_FreeSurface(surface);
return texture;")

   (c-exported-function
    :void "Clear" 'clear! ()
    "Clears the screen to the current draw color."
    "SDL_RenderClear(g_renderer);")
   (c-exported-function
    :void "SetDrawColor" 'set-draw-color!
    (parameters :u8 "r" 'r
		:u8 "g" 'g
		:u8 "b" 'b
		:u8 "a" 'a)
    "Sets the current draw color."
    "SDL_SetRenderDrawColor(g_renderer, r, g, b, a);")
   (c-exported-function
    :void "DrawRect" 'draw-rect!
    (parameters :s32 "x" 'x
		:s32 "y" 'y
		:s32 "w" 'w
		:s32 "h" 'h)
    "Draws a rect outline with the current draw color."
    "SDL_Rect rect = { .x=x, .y=y, .w=w, .h=h };
SDL_RenderDrawRect(g_renderer, &rect);")
   (c-exported-function
    :void "FillRect" 'fill-rect!
    (parameters :s32 "x" 'x
		:s32 "y" 'y
		:s32 "w" 'w
		:s32 "h" 'h)
    "Draws a filled rect with the current draw color."
    "SDL_Rect rect = { .x=x, .y=y, .w=w, .h=h };
SDL_RenderFillRect(g_renderer, &rect);")
   (c-exported-function
    :void "DrawTexture" 'draw-texture!
    (parameters :texture "texture" 'texture
		:int "sx" 'sx :int "sy" 'sy :int "sw" 'sw :int "sh" 'sh
		:int "dx" 'dx :int "dy" 'dy :int "dw" 'dw :int "dh" 'dh)
    "Draws a portion of texture described by source rect to a section of the screen
described by destination rect."
    "SDL_Rect src = { .x=sx, .y=sy, .w=sw, .h=sh };
SDL_Rect dest = { .x=dx, .y=dy, .w=dw, .h=dh };
SDL_RenderCopy(g_renderer, texture, &src, &dest);")

   (c-exported-function
    :void "TextureColorMod" 'texture-color-mod!
    (parameters :texture "texture" 'texture
		:u8 "r" 'r
		:u8 "g" 'g
		:u8 "b" 'b)
    "Mod the texture's pixel colors with the provided color."
    "SDL_SetTextureColorMod(texture, r, g, b);")
   (c-exported-function
    :void "Present" 'present! ()
    "Flip the display buffer."
    "SDL_RenderPresent(g_renderer);")

   (c-exported-function
    :u32 "BufferedAudioBytes" 'buffered-audio-bytes! ()
    "Return the number of bytes currently buffered."
    "return SDL_GetQueuedAudioSize(g_audio_device_id);")
   (c-exported-function
    :int "BufferAudio" 'buffer-audio!
    (parameters :u8* "bytes" 'bytes
		:u32 "num_bytes" 'num-bytes)
    "Fills the audio buffer with the provided bytes array.
Returns non-zero on error."
    "return SDL_QueueAudio(g_audio_device_id, bytes, num_bytes);")
   (c-exported-function
    :void "PauseAudio" 'pause-audio!
    ()
    "Pauses playback of the audio device."
    "SDL_PauseAudioDevice(g_audio_device_id, 1);")
   (c-exported-function
    :void "PlayAudio" 'play-audio!
    ()
    "Resumes playback of the audio device."
    "SDL_PauseAudioDevice(g_audio_device_id, 0);")
   (c-exported-function
    :void "Delay" 'delay!
    (parameters :int "milliseconds" 'milliseconds)
    "Delay the thread for at least the provided number of milliseconds."
    "SDL_Delay(milliseconds);")

   (c-exported-function
    :event-type "NextEvent" 'next-event%!
    (parameters (out-parameter-type :int) "scancode" 'scancode
		(out-parameter-type :int) "mouse_button" 'mouse-button
		(out-parameter-type :int) "clicks" 'clicks
		(out-parameter-type :int) "mouse_x" 'mouse-x
		(out-parameter-type :int) "mouse_y" 'mouse-y
		:char* "text" 'text)
    "Polls for the next input event and returns the type of the event.
If no events are left on the event queue, NO_EVENT is returned.
The following out parameters will be filled based on the type:
  NO_EVENT/QUIT: N/A
  KEYDOWN/KEYUP: scancode (note: repeated key presses are ignored)
  TEXTINPUT: text
  MOUSEDOWN/MOUSEUP: mouse_button, clicks
  MOUSEWHEEL: mouse_x,mouse_y (representing horizontal and vertical scroll amount)
  MOUSEMOVE: mouse_x,mouse_y (representing pixel position)"
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
    :char* "ScancodeName" 'scancode-name
    (parameters :scancode "scancode" 'scancode)
    "Returns the name of the scancode."
    "return SDL_GetScancodeName(scancode);")

   (c-exported-function
    :scancode "GetScancodeFromName" 'scancode-from-name
    (parameters :char* "name" 'name)
    "Returns the scancode with the given name."
    "return SDL_GetScancodeFromName(name);")

   (c-exported-function
    :u32 "ElapsedMilliseconds" 'elapsed-milliseconds
    ()
    "Number of milliseconds elapsed since the SDL timer subsytem started."
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
		(cdr (assoc :parameters c-function))
		(cdr (assoc :c-specifiers c-function))
		(cdr (assoc :documentation c-function))))
	     (sdl-wrapper-c-functions)))

    (lines
     (mapcar (lambda (c-function)
	       (c-function-definition
		(cdr (assoc :type c-function))
		(cdr (assoc :c-name c-function))
		(cdr (assoc :parameters c-function))
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
    (assemble-sdl-wrapper-c-code!)
    (let* ((success t)
	   (error-string
	     (with-output-to-string (s)
	       (handler-case (build-dll! :output-stream s :error-output-stream s)
		 (error () (setq success nil))))))
      (unless success
	(error "Could not make dll~%~S" error-string)))
    (load-shared-object dll-fullname)
    (define-sdl-wrapper-alien-routines!)))
