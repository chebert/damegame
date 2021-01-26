;;;; damegame.lisp

(in-package #:damegame)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-keyword (symbol)
    (intern (symbol-name symbol) (find-package :keyword)))
  (defun symbolicate (&rest symbols)
    (intern (apply 'concatenate 'string (mapcar 'symbol-name symbols))))
  (defvar *tests* (make-hash-table)))

(defmacro deftest (name &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (setf (gethash ',name *tests*) ',name)))
(defmacro undeftest (name &rest unused)
  (declare (ignore unused))
  `(remhash ',name *tests*))


(defmacro fn (&body body)
  "Creates an anaphoric lambda with optional arguments % %% %%%."
  `(lambda (&optional % %% %%%)
     (declare (ignorable % %% %%%))
     ,@body))

(defun run-tests! ()
  (maphash (fn (funcall %)) *tests*))

(defmacro defcloss (name direct-superclasses &rest documentation-and-slot-names)
  "Defcloss provides a similar interface to defstruct (with the addition of direct-superclasses)."
  (let* ((first (first documentation-and-slot-names))
	 (documentation (when (stringp first)
			  first))
	 (slot-names (if (stringp first)
			 (rest documentation-and-slot-names)
			 documentation-and-slot-names))
	 (slot-keys (mapcar 'make-keyword slot-names))
	 (slot-reader-names (mapcar (fn (symbolicate name '- %)) slot-names))
	 (slot-forms (mapcar (fn (list % :initarg %% :reader %)) slot-reader-names slot-keys)))
    `(progn
       (defclass ,name ,direct-superclasses
	 ,slot-forms
	 ,@ (when documentation (list (list :documentation documentation))))
       (defun ,(symbolicate name '-p) (instance)
	 (typep instance ',name))
       ',name)))

;; From CBaggers' Swank.Live
(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
   error. Remember to hit C in slime or pick the restart so
   errors don't kill the app."
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

;; From CBaggers' Swank.Live
(defun update-swank! ()
  "Called from within the main loop, this keep the lisp repl
   working while cepl runs"
  (let ((connection (or swank::*emacs-connection*
                        (swank::default-connection))))
    (when connection
      (swank::handle-requests connection t))))


(defparameter *fps* 60 "The number of frames/second.")
(defparameter *width* 1024 "The width of the window.")
(defparameter *height* 768 "The height of the window.")
(defparameter *audio-frequency* 48000 "The sample playback frequency for the audio buffer.")
(defparameter *audio-channels* 2
  "The number of audio channels (e.g. 1 for mono, 2 for stereo, etc.)")

(defmacro nlet (name bindings &body body)
  "Named let for convenient recursion."
  `(labels ((,name ,(mapcar 'first bindings)
	      ,@body))
     (,name ,@(mapcar 'second bindings))))

(defmacro check (form)
  "Throws a helpful error if FORM is not truthy."
  (let ((result (gensym)))
    `(let ((,result ,form))
       (unless ,result
	 (continuable
	   (error "(CHECK ~S) failed" ,form)))
       :check)))

(defmacro checkeql (a b)
  "Throws a helpful error if a is not EQL to b."
  (let ((a-val (gensym))
	(b-val (gensym)))
    `(let ((,a-val ,a)
	   (,b-val ,b))
       (unless (eql ,a-val ,b-val)
	 (continuable
	   (error "~S => ~S does not EQL ~S => ~S" ',a ,a-val ',b ,b-val)))
       :check)))

(defun for-each-input-event! (fn)
  "Iterate over each event in the sdl-event queue, applying fn to each event.
Removes events from the queue."
  (loop while (let ((event (next-event!)))
		(funcall fn event)
		event)))

(defvar *events* ())
(defun event! (event)
  (push event *events*))

(defvar *fonts* (make-hash-table))
(defvar *textures* (make-hash-table))

(defcloss event-font-opened ()
  font-id)

(defun load-font! (font-id path size)
  "Opens the font and stores it in *fonts*, closing any existing font."
  (let* ((existing-font (gethash font-id *fonts*)))
    ;; If another font with the same id is already open, close it first.
    (when existing-font
      (close-font! existing-font))
    ;; Open the font and add it to the *fonts* hash-table
    (setf (gethash font-id *fonts*) (open-font! path size))
    (notify-handlers! (make-instance
		       'event-font-opened :font-id font-id))))

(defcloss event-texture-loaded ()
  texture-id)
(defun load-text-texture! (id font-id text)
  "Creates the text texture and stores it in *textures*, destroying any existing texture."
  (let* ((font (gethash font-id *fonts*))
	 (existing-texture (gethash id *textures*)))
    (cond
      ((null font)
       ;; If the font doesn't exist, warn and don't do anyting else
       (warn "Unable to find font ~S when creating text-texture ~S ~S" font-id id text))
      (t
       ;; If another texture with the same id already exists, free it
       (when existing-texture
	 (free-texture! existing-texture))
       ;; Create the texture and add it to the textures hash-table
       (setf (gethash id *textures*)
	     (create-text-texture! font text))
       (notify-handlers! (make-instance
			  'event-texture-loaded :texture-id id))))))

(defun draw-full-texture! (texture dx dy)
  (when texture
    (let* ((w (texture-width texture))
	   (h (texture-height texture)))
      (draw-texture! texture 0 0 w h dx dy w h))))

(defparameter *grid-size* 20)

(defun v2 (x y) (vector x y))
(defun g2 (grid-x grid-y) (v2 (* grid-x *grid-size*) (* grid-y *grid-size*)))
(defun rect (x y w h) (vector x y w h))
(defun x (v) (aref v 0))
(defun y (v) (aref v 1))
(defun w (r) (aref r 2))
(defun h (r) (aref r 3))

(defun v+ (&rest vs)
  (let ((result (v2 0 0)))
    (mapc (fn (setq result (v2 (+ (x result) (x %))
			       (+ (y result) (y %)))))
	  vs)
    result))

(defun alist (&rest plist)
  "Create an association list ((id . value) (id2 . value2)...)
From the plist (id value id2 value2 ...)"
  (nlet rec ((plist plist)
	     (result ()))
    (if plist
	(let ((key (first plist))
	      (value (second plist))
	      (rest (rest (rest plist))))
	  (rec rest (acons key value result)))
	result)))
(defun aval (key alist &optional not-present)
  "Get the value from the alist assuming it is there. Returns not-present if it isn't."
  (let ((pair (assoc key alist)))
    (if pair
	(cdr pair)
	not-present)))
(defun aremove (alist &rest keys)
  "Return a new alist with keys removed from alist."
  (remove-if (fn (member % keys))
	     alist
	     :key 'car))
(defun aset (key value alist)
  "Return a new alist with the value associated with key added or replaced."
  (acons key value (aremove alist key)))
(defun akeys (alist)
  "Return a list of all keys in alist."
  (mapcar 'car alist))
(defun amerge (old new)
  "Return a new alist with the keys of old and new, where conflicts favor the new alist."
  (nconc (apply 'aremove old (akeys new))
	 new))
(defun amap (fn alist)
  "Apply fn to each key and value of alist returning a list of the results."
  (mapcar (fn (funcall fn (car %) (cdr %)))
	  alist))


;; TODO: If we start adding to/removing from *DRAWINGS* frequently, then
;; something like a binary tree may be better
(defvar *drawings* ()
  "An association list of drawings.")

(defun drawing (layer fn)
  (alist :layer layer
	 :fn fn))

(defun sort-drawings-by-layer! ()
  "Sort *drawings* by layer."
  (setq *drawings* (sort *drawings* #'< :key (fn (aval :layer (cdr %))))))
(defun remove-drawing! (drawing-id)
  "Removes the associated drawing from *drawings*"
  (setq *drawings* (aremove *drawings* drawing-id))
  (sort-drawings-by-layer!))

(defun color (r g b a)
  "Create an RGBA representation of a color"
  (vector r g b a))
(defun r (color)
  "red component of color"
  (aref color 0))
(defun g (color)
  "green component of color"
  (aref color 1))
(defun b (color)
  "blue component of color"
  (aref color 2))
(defun a (color)
  "alpha component of color"
  (aref color 3))

(defun add-drawing! (id drawing)
  "Adds drawing *drawings* and sorts *drawings* by layer."
  (setq *drawings* (aset id drawing *drawings*))
  (sort-drawings-by-layer!))

(defvar *quit?* nil
  "When true the main-loop! will terminate.")

(defvar *mouse-x* 0 "Mouse pixel x position measured from the left of the window.")
(defvar *mouse-y* 0 "Mouse pixel y position measured from the top of the window.")

(defvar *controls* ()
  "An association list of control-id to control of all of the currently active controls.")

(defcloss control ()
  "A control is a rectangle which recieves (currently mouse-only) inputs."
  rect
  drawing-id
  hovered?
  pressed?)

(defun fill-rect-drawing (layer color rect)
  (drawing layer
	   (fn
	     (set-draw-color! (r color) (g color) (b color) (a color))
	     (fill-rect! (x rect) (y rect) (w rect) (h rect)))))

(defun add-control! (id control)
  "Adds control to *controls* and adds a drawing of the control."
  (setq *controls* (aset id control *controls*))
  (add-drawing! (control-drawing-id control)
		;; TODO: control drawing (based on layer, pressed? and hovered?
		(fill-rect-drawing 1 (color 0 0 0 255) (control-rect control))))

(defun point-in-rect? (v2 r)
  "True if the given v2 point is inside of the rect top-left inclusive, bottom-right exclusive."
  (and (<= (x r) (x v2) (1- (+ (x r) (w r))))
       (<= (y r) (y v2) (1- (+ (y r) (h r))))))

(defun control-handle-mouse-move! (control)
  "Process the effects of a mouse-move event on control."
  (let* ((rect (control-rect control))
	 (hovered? (control-hovered? control))
	 (pressed? (control-pressed? control))
	 (in-rect? (point-in-rect? (v2 *mouse-x* *mouse-y*) rect)))
    (cond
      ((and (not hovered?)
	    in-rect?)
       (setf (slot-value control 'control-hovered?) t)
       (add-drawing! (control-drawing-id control)
		     (fill-rect-drawing 1 (color 255 0 255 255) rect)))
      ((and hovered?
	    (not in-rect?))
       (setf (slot-value control 'control-hovered?) nil)
       (when (not pressed?)
	 (add-drawing! (control-drawing-id control)
		       (fill-rect-drawing 1 (color 0 0 0 255) rect)))))))

(defun control-handle-mouse-down! (control)
  "Process the effects of a left-mouse-button press on control."
  (when (control-hovered? control)
    (setf (slot-value control 'control-pressed?) t)
    (add-drawing! (control-drawing-id control)
		  (fill-rect-drawing 1 (color 255 0 0 255) (control-rect control)))))

(defvar *event-handlers* (make-hash-table)
  "A hash-table of runtime-created event-handlers.")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *compiled-event-handlers* (make-hash-table)
    "A hash-table of compile-time created event-handlers."))

(defcloss event-control-clicked ()
  control-id)

(defun notify-handlers! (event)
  "Call the compiled & runtime event handlers with the given event."
  (maphash (fn (funcall %% event %)) *compiled-event-handlers*)
  (maphash (fn (funcall %% event %)) *event-handlers*))
(defun register-handler! (id fn)
  "Add/replace the handler in *event-handlers*"
  (setf (gethash id *event-handlers*) fn))
(defun remove-handler! (id)
  "Remove the handler from *event-handlers*"
  (remhash id *event-handlers*))
(defmacro defhandler (name (event) &body body)
  "Add/replace the handler NAME in *compiled-event-handlers*"
  (let ((handler-id (gensym)))
    `(setf (gethash ',name *compiled-event-handlers*)
	   (lambda (,event ,handler-id)
	     (declare (ignorable ,event ,handler-id))
	     ,@body))))
(defmacro undefhandler (name &rest rest)
  "Remove the handler NAME from *compiled-event-handlers*"
  (declare (ignore rest))
  `(remhash ',name *compiled-event-handlers*))

(defun control-handle-mouse-up! (control-id control)
  "Process the effects on control of the left mouse button being released"
  (when (and (control-pressed? control) (control-hovered? control))
    (notify-handlers! (make-instance
		       'event-control-clicked
		       :control-id control-id))
    (setf (slot-value control 'control-pressed?) nil)
    (add-drawing! (control-drawing-id control)
		  (fill-rect-drawing 1 (color 255 0 255 255) (control-rect control)))))

(defun update! ()
  "Handles events, handles input events,
updates based on timestep, and renders to the screen."
  (mapcar 'funcall *commands*)
  (setq *commands* ())

  (let ((events (reverse *events*)))
    (setq *events* ())
    (mapcar 'notify-handlers! events))

  ;; Handle input events.
  (for-each-input-event!
   (fn (typecase %
	 (event-quit (setq *quit?* t))
	 (event-mousemove
	  (setq *mouse-x* (event-mousemove-x %)
		*mouse-y* (event-mousemove-y %))
	  (load-text-texture! :mouse-pos :font (mouse-pos-text))
	  (mapcar (fn (control-handle-mouse-move! (cdr %))) *controls*))
	 (event-mousedown
	  (when (eql :left (event-mousedown-button %))
	    (mapcar (fn (control-handle-mouse-down! (cdr %))) *controls*)))
	 (event-mouseup
	  (when (eql :left (event-mouseup-button %))
	    (mapcar (fn (control-handle-mouse-up! (car %) (cdr %))) *controls*))))))

  ;; Update based on time-step

  ;; Render to the screen
  (set-draw-color! 0 0 0 255)
  (clear!)
  (amap (fn (funcall (aval :fn %%))) *drawings*)
  (present!)
  
  (update-swank!))

(defun milliseconds/frame ()
  "Duration of a frame in milliseconds."
  (/ 1000 *fps*))
(defun frame-time-elapsed? (last-update-milliseconds current-milliseconds)
  "True if the frame time has elapsed."
  (> (frame-milliseconds-elapsed last-update-milliseconds current-milliseconds)
     (milliseconds/frame)))

(defun test-frame-time-elapsed? ()
  (check (frame-time-elapsed? 60 78)))

(defun frame-milliseconds-elapsed (frame-start-milliseconds current-milliseconds)
  "Returns milliseconds elapsed since the start of the frame."
  (- current-milliseconds frame-start-milliseconds))
(defun frame-milliseconds-remaining (frame-start-milliseconds current-milliseconds)
  "Returns the (0+) number of milliseconds remaining in the current frame."
  ;; NOTE: When I click on the X to close the window, the frame time elapsed becomes
  ;; larger than the milliseconds/frame. (max 0 ...) is a temporary fix to
  ;; keep it from being negative and lasting forever.
  ;; Something like pausing when the window loses focus seems like a good permanent fix.
  (max 0
       (truncate
	(- (milliseconds/frame)
	   (frame-milliseconds-elapsed frame-start-milliseconds current-milliseconds)))))

(defun test-frame-time-remaining ()
  (checkeql (frame-milliseconds-remaining 32 40) 8)
  (checkeql (frame-milliseconds-remaining 32 70) 0))

(defun main-loop! ()
  "Loops *FPS* times per second, calling update! until *quit?* is true."
  (let ((last-update-milliseconds (elapsed-milliseconds)))
    (loop
      until *quit?*
      do
	 (when (frame-time-elapsed? last-update-milliseconds (elapsed-milliseconds))
	   (setq last-update-milliseconds (elapsed-milliseconds))
	   (continuable (update!))
	   (delay! (frame-milliseconds-remaining last-update-milliseconds
						 (elapsed-milliseconds)))))))

(defcloss event-initialization-finished ())

(defun main! ()
  "Entry point into the application. Recompiles the SDL-Wrapper, creates the window, 
and starts the update loop, then afterwards closes SDL and cleans up the application."
  (recompile-sdl-wrapper-dll!)
  (unwind-protect 
       (progn
	 (setq *quit?* nil)
	 (clrhash *fonts*)
	 (clrhash *textures*)
	 (clrhash *event-handlers*)
	 (clrhash *buttons*)
	 (setq *drawings* ())
	 (setq *events* ())
	 (setq *controls* ())
	 (start! *width* *height* *audio-frequency* *audio-channels*)
	 (event! (make-instance 'event-initialization-finished))
	 (main-loop!))
    (maphash (fn (close-font! %%)) *fonts*)
    (maphash (fn (free-texture! %%)) *textures*)
    (quit!)))

(defun event-font-opened? (event font-id)
  "True if the event is an event-font-opened for the font-id"
  (and (event-font-opened-p event)
       (eql font-id (event-font-opened-font-id event))))
(defun event-control-clicked? (event control-id)
  "True if event is control-id's control being clicked."
  (and (event-control-clicked-p event)
       (eql control-id (event-control-clicked-control-id event))))
(defun event-texture-loaded? (event texture-id)
  "True if event is texture-id's texture being created."
  (and (event-texture-loaded-p event)
       (eql texture-id (event-texture-loaded-texture-id event))))

(defun draw-full-texture-id! (texture-id pos)
  (draw-full-texture! (gethash texture-id *textures*) (x pos) (y pos)))

(defun full-texture-drawing (layer texture-id pos &optional color)
  (drawing
   layer
   (fn
     (when color
       (set-draw-color! (r color) (g color) (b color) (a color)))
     (draw-full-texture! (gethash texture-id *textures*) (x pos) (y pos)))))

(defun create-text-texture-drawing! (drawing-id texture-id font-id text layer pos)
  "Loads the text texture into *textures* and adds the drawing to *drawings*."
  (load-text-texture! texture-id font-id text)
  (add-drawing! drawing-id (full-texture-drawing layer texture-id pos)))

(defun register-one-off-handler! (test-fn handle-fn)
  "Registers an event handler that will be removed once handled.
Only calls handle-fn if test-fn returns a truthy value.
Test-fn and handle-fn are both functions of event."
  (register-handler!
   (gensym)
   (lambda (event handler-id)
     (when (funcall test-fn event)
       (funcall handle-fn event)
       (remove-handler! handler-id)))))

(defun texture-rect (pos texture)
  "Returns a rect positioned at pos with the same dimensions as texture."
  (rect (x pos) (y pos) (texture-width texture) (texture-height texture)))

(defcloss button-spec ()
  control-id
  font-id
  texture-id
  text-drawing-id
  text bottom-layer pos)

(defvar *buttons* (make-hash-table))
(defun create-button! (button-id button-spec)
  "Creates a button from the button spec."
  (let ((texture-id (button-spec-texture-id button-spec))
	(text-drawing-id (button-spec-text-drawing-id button-spec))
	(pos (button-spec-pos button-spec))
	(text (button-spec-text button-spec)))
    (register-one-off-handler!
     (fn (event-texture-loaded? % texture-id))
     (fn
       (add-control!
	(button-spec-control-id button-spec)
	(make-instance
	 'control
	 :rect (texture-rect pos (gethash texture-id *textures*))
	 :hovered? nil :pressed? nil
	 :drawing-id (gensym)))
       (setf (gethash button-id *buttons*) button-spec)))

    (let* ((font-id (button-spec-font-id button-spec))
	   (create-fn (fn (create-text-texture-drawing! text-drawing-id texture-id font-id
							text
							(1+ (button-spec-bottom-layer button-spec))
							pos))))
      (if (gethash font-id *fonts*)
	  (funcall create-fn)
	  ;; If font isn't open, create a handler to wait for it
	  (progn
	    (warn "Trying to create button when the font is not loaded ~S" button-spec)
	    (register-one-off-handler!
	     (fn (event-font-opened? % font-id))
	     create-fn))))))

(defun unload-texture! (id)
  "Frees the texture and removes it from *textures*."
  (let* ((texture (gethash id *textures*)))
    (when texture
      (free-texture! texture)
      (remhash id *textures*))))

(defun remove-control! (id)
  "Removes the control from *controls* and removes its drawing from *drawings*."
  (let* ((control (aval id *controls*)))
    (when control
      (remove-drawing! (control-drawing-id control))
      (setq *controls* (aremove *controls* id)))))

(defun destroy-button! (button-id)
  "Removes/unloads the button's control, textures, and drawings."
  (let* ((button-spec (gethash button-id *buttons*))
	 (texture-id (button-spec-texture-id button-spec))
	 (control-id (button-spec-control-id button-spec))
	 (text-drawing-id (button-spec-text-drawing-id button-spec)))
    (remove-drawing! text-drawing-id)
    (unload-texture! texture-id)
    (remove-control! control-id)
    (remhash button-id *buttons*)))

(defmacro defbutton (name text pos layer font-id &body on-click)
  (let ((event (gensym)))
    `(progn
       (defhandler ,(symbolicate 'initialize- name '-button) (,event)
	 (when (event-font-opened? ,event ,font-id)
	   (add-button! ',name ,text ,pos ,layer ,font-id (fn ,@on-click))))
       (let ((button-spec (gethash ',name *buttons*)))
	 (when button-spec
	   (destroy-button! ',name)
	   (add-button! ',name ,text ,pos ,layer ,font-id (fn ,@on-click)))))))
(defmacro undefbutton (name &rest args)
  (declare (ignore args))
  `(undefhandler ,(symbolicate 'initialize- name '-button)))

(defhandler handle-intialization-finished! (event)
  (when (event-initialization-finished-p event)
    (load-font! :font "DroidSansMono.ttf" 16)))

(defvar *commands* ())

(defmacro command! (&body body)
  `(push (fn ,@body) *commands*))

(defun texture-right-aligned (x texture-id)
  (- x (texture-width (gethash texture-id *textures*))))

(defun texture-right-alignment (texture-id pos)
  (v2 (texture-right-aligned (x pos) texture-id) (y pos)))

(defun draw-full-texture-id-right-aligned! (texture-id pos)
  (draw-full-texture-id! texture-id (texture-right-alignment texture-id pos)))

(defstruct cpu
  a b c d e f h l sp pc flag)

(defun g1 (value) (* value *grid-size*))

(defun cpu-initial ()
  (make-cpu :a 0 :b 0 :c 0 :d 0 :e 0 :f 0 :h 0 :l 0 :sp 0 :pc 0 :flag 0))

(defvar *cpu* (cpu-initial))
(defvar *cpu-previous* (cpu-initial))
(defun cpu-state-left-column (cpu-pos)
  (+ (g1 6) (x cpu-pos)))
(defun cpu-state-right-column (cpu-pos)
  (+ (g1 16) (x cpu-pos)))
(defparameter *cpu-state-layer* 1)

(defun grey (v) (color v v v 255))
(defun white () (grey 255))
(defun black () (grey 0))
(defun red (&optional (v 255)) (color v 0 0 255))
(defun green (&optional (v 255)) (color 0 v 0 255))
(defun blue (&optional (v 255)) (color 0 0 v 255))
(defun yellow (&optional (v 255)) (color v v 0 255))

(defun set-color! (color)
  (set-draw-color! (r color) (g color) (b color) (a color)))
(defun set-texture-color! (texture-id color)
  (let ((texture (gethash texture-id *textures*)))
    (when texture
      (texture-color-mod! texture (r color) (g color) (b color)))))

(defun draw-cpu-visualization! (cpu-visualization)
  (let* ((cpu-pos (funcall (aval :pos-fn cpu-visualization))))
    (set-color! (white))
    (draw-rect! (- (x cpu-pos) (/ *grid-size* 2))
		(- (y cpu-pos) (/ *grid-size* 2))
		(g1 19)
		(g1 11))
    (draw-full-texture-id! (aval :title-texture-id cpu-visualization)
			   (v+ cpu-pos (v2 0 (truncate (g1 -1.5)))))
    (let* ((cpu (funcall (aval :cpu-fn cpu-visualization)))
	   (cpu-colors (aval :colors cpu-visualization))
	   
	   (left-column (cpu-state-left-column cpu-pos))
	   (right-column (cpu-state-right-column cpu-pos))
	   (y (y cpu-pos)))
      (draw-full-texture-id-right-aligned! :zero (v2 left-column y))
      (draw-full-texture-id-right-aligned! :subtraction (v2 right-column y))
      (let ((texture-id (flag-state-texture-id (cpu-zero? cpu))))
	(set-texture-color! texture-id (aval :zero cpu-colors (white)))
	(draw-full-texture-id! texture-id (v2 left-column y)))
      (let ((texture-id (flag-state-texture-id (cpu-subtraction? cpu))))
	(set-texture-color! texture-id (aval :subtraction cpu-colors (white)))
	(draw-full-texture-id! texture-id (v2 right-column y)))
      
      (incf y *grid-size*)
      (draw-full-texture-id-right-aligned! :half-carry (v2 left-column y))
      (draw-full-texture-id-right-aligned! :carry (v2 right-column y))
      (let ((texture-id (flag-state-texture-id (cpu-half-carry? cpu))))
	(set-texture-color! texture-id (aval :half-carry cpu-colors (white)))
	(draw-full-texture-id! texture-id (v2 left-column y)))
      (let ((texture-id (flag-state-texture-id (cpu-carry? cpu))))
	(set-texture-color! texture-id (aval :carry cpu-colors (white)))
	(draw-full-texture-id! texture-id (v2 right-column y)))

      (incf y *grid-size*)
      (let ((c1 (+ (g1 1) (x cpu-pos)))
	    (c2 (+ (g1 8) (x cpu-pos)))
	    (c3 (+ (g1 15) (x cpu-pos))))
	(incf y *grid-size*)
	(draw-full-texture-id-right-aligned! :a-register (v2 c1 y))
	(draw-full-texture-id-right-aligned! :f-register (v2 c2 y))
	(draw-full-texture-id-right-aligned! :af-register (v2 c3 y))
	(set-texture-color! (aval :a cpu-visualization) (aval :a cpu-colors (white)))
	(draw-full-texture-id! (aval :a cpu-visualization) (v2 c1 y))
	(set-texture-color! (aval :f cpu-visualization) (aval :f cpu-colors (white)))
	(draw-full-texture-id! (aval :f cpu-visualization) (v2 c2 y))
	(set-texture-color! (aval :af cpu-visualization) (aval :af cpu-colors (white)))
	(draw-full-texture-id! (aval :af cpu-visualization) (v2 c3 y))

	(incf y (g1 1))
	(draw-full-texture-id-right-aligned! :b-register (v2 c1 y))
	(draw-full-texture-id-right-aligned! :c-register (v2 c2 y))
	(draw-full-texture-id-right-aligned! :bc-register (v2 c3 y))
	(set-texture-color! (aval :b cpu-visualization) (aval :b cpu-colors (white)))
	(draw-full-texture-id! (aval :b cpu-visualization) (v2 c1 y))
	(set-texture-color! (aval :c cpu-visualization) (aval :c cpu-colors (white)))
	(draw-full-texture-id! (aval :c cpu-visualization) (v2 c2 y))
	(set-texture-color! (aval :bc cpu-visualization) (aval :bc cpu-colors (white)))
	(draw-full-texture-id! (aval :bc cpu-visualization) (v2 c3 y))

	(incf y (g1 1))
	(draw-full-texture-id-right-aligned! :d-register (v2 c1 y))
	(draw-full-texture-id-right-aligned! :e-register (v2 c2 y))
	(draw-full-texture-id-right-aligned! :de-register (v2 c3 y))
	(set-texture-color! (aval :d cpu-visualization) (aval :d cpu-colors (white)))
	(draw-full-texture-id! (aval :d cpu-visualization) (v2 c1 y))
	(set-texture-color! (aval :e cpu-visualization) (aval :e cpu-colors (white)))
	(draw-full-texture-id! (aval :e cpu-visualization) (v2 c2 y))
	(set-texture-color! (aval :de cpu-visualization) (aval :de cpu-colors (white)))
	(draw-full-texture-id! (aval :de cpu-visualization) (v2 c3 y))
	
	(incf y (g1 1))
	(draw-full-texture-id-right-aligned! :h-register (v2 c1 y))
	(draw-full-texture-id-right-aligned! :l-register (v2 c2 y))
	(draw-full-texture-id-right-aligned! :hl-register (v2 c3 y))
	(set-texture-color! (aval :h cpu-visualization) (aval :h cpu-colors (white)))
	(draw-full-texture-id! (aval :h cpu-visualization) (v2 c1 y))
	(set-texture-color! (aval :l cpu-visualization) (aval :l cpu-colors (white)))
	(draw-full-texture-id! (aval :l cpu-visualization) (v2 c2 y))
	(set-texture-color! (aval :hl cpu-visualization) (aval :hl cpu-colors (white)))
	(draw-full-texture-id! (aval :hl cpu-visualization) (v2 c3 y)))

      (incf y *grid-size*)
      (let ((column (+ (g1 11) (x cpu-pos))))
	(incf y (g1 1))
	(draw-full-texture-id-right-aligned! :stack-pointer (v2 column y))
	(set-texture-color! (aval :sp cpu-visualization) (aval :sp cpu-colors (white)))
	(draw-full-texture-id! (aval :sp cpu-visualization) (v2 column y))

	(incf y (g1 1))
	(draw-full-texture-id-right-aligned! :program-counter (v2 column y))
	(set-texture-color! (aval :pc cpu-visualization) (aval :pc cpu-colors (white)))
	(draw-full-texture-id! (aval :pc cpu-visualization) (v2 column y))))))

(defun initialize-cpu-visualization! (drawing-id cpu-visualization-fn)
  (let ((vis (funcall cpu-visualization-fn)))
    (load-text-texture! (aval :title-texture-id vis)
			:font
			(aval :title vis))
    (update-cpu-visualization! vis))
  (add-drawing! drawing-id
		(drawing *cpu-state-layer*
			 (fn (draw-cpu-visualization! (funcall cpu-visualization-fn))))))

(defhandler load-cpu-visualization (event)
  (when (event-font-opened? event :font)
    (load-text-texture! :no :font "[No]")
    (load-text-texture! :yes :font "[Yes]")
    (load-text-texture! :zero :font "Zero? ")
    (load-text-texture! :subtraction :font "Subtraction? ")
    (load-text-texture! :half-carry :font "Half-carry? ")
    (load-text-texture! :carry :font "Carry? ")
    (load-text-texture! :a-register :font "A ")
    (load-text-texture! :f-register :font "F ")
    (load-text-texture! :af-register :font "AF ")
    (load-text-texture! :b-register :font "B ")
    (load-text-texture! :c-register :font "C ")
    (load-text-texture! :bc-register :font "BC ")
    (load-text-texture! :d-register :font "D ")
    (load-text-texture! :e-register :font "E ")
    (load-text-texture! :de-register :font "DE ")
    (load-text-texture! :h-register :font "H ")
    (load-text-texture! :l-register :font "L ")
    (load-text-texture! :hl-register :font "HL ")
    (load-text-texture! :stack-pointer :font "Stack Pointer: ")
    (load-text-texture! :program-counter :font "Program Counter: ")

    (initialize-cpu-visualization! :cpu-visualization (fn *cpu-visualization*))
    (initialize-cpu-visualization! :cpu-visualization-previous (fn *cpu-visualization-previous*))))

(defun flag-state-texture-id (set?)
  (if set? :yes :no))

(defun cpu-visualization (title pos-fn cpu-fn)
  (alist
   :title title
   :cpu-fn cpu-fn
   :title-texture-id (gensym)
   :pos-fn pos-fn
   :pc (gensym)
   :sp (gensym)
   :hl (gensym)
   :l (gensym)
   :h (gensym)
   :de (gensym)
   :e (gensym)
   :d (gensym)
   :bc (gensym)
   :c (gensym)
   :b (gensym)
   :af (gensym)
   :f (gensym)
   :a (gensym)
   :colors ()))

(defun update-cpu-visualization! (cpu-visualization)
  (let* ((cpu (funcall (aval :cpu-fn cpu-visualization))))
    (load-text-texture! (aval :a cpu-visualization) :font (register8-text (cpu-a cpu)))
    (load-text-texture! (aval :f cpu-visualization) :font (register8-text (cpu-f cpu)))
    (load-text-texture! (aval :af cpu-visualization) :font (register16-text (cpu-af cpu)))
    (load-text-texture! (aval :b cpu-visualization) :font (register8-text (cpu-b cpu)))
    (load-text-texture! (aval :c cpu-visualization) :font (register8-text (cpu-c cpu)))
    (load-text-texture! (aval :bc cpu-visualization) :font (register16-text (cpu-bc cpu)))
    (load-text-texture! (aval :d cpu-visualization) :font (register8-text (cpu-d cpu)))
    (load-text-texture! (aval :e cpu-visualization) :font (register8-text (cpu-e cpu)))
    (load-text-texture! (aval :de cpu-visualization) :font (register16-text (cpu-de cpu)))
    (load-text-texture! (aval :h cpu-visualization) :font (register8-text (cpu-h cpu)))
    (load-text-texture! (aval :l cpu-visualization) :font (register8-text (cpu-l cpu)))
    (load-text-texture! (aval :hl cpu-visualization) :font (register16-text (cpu-hl cpu)))
    (load-text-texture! (aval :sp cpu-visualization) :font
			(let ((*number-base* :hexadecimal))
			  (register16-text (cpu-sp cpu))))
    (load-text-texture! (aval :pc cpu-visualization) :font
			(let ((*number-base* :hexadecimal))
			  (register16-text (cpu-pc cpu))))))

(defparameter *number-base* :hexadecimal)

(defun s8 (value)
  (if (> value 127)
      (- value 256)
      value))
(defun s16 (value)
  (if (>= value (expt 2 15))
      (- value (expt 2 16))
      value))

(deftest test-s8
  (checkeql (s8 #x7f) (1- (expt 2 7)))
  (checkeql (s8 #x0) 0)
  (checkeql (s8 #xff) -1))
(deftest test-s16
  (checkeql (s16 #x7fff) (1- (expt 2 15)))
  (checkeql (s16 #x0) 0)
  (checkeql (s16 #xffff) -1))

(defun register8-text (register)
  (ecase *number-base*
    (:hexadecimal
     (format nil "0x~2,'0x" register))
    (:signed
     (format nil "~d" (s8 register)))
    (:unsigned
     (format nil "~d" register))
    (:binary
     (format nil "0b~8,'0b" register))))
(defun register16-text (register)
  (ecase *number-base*
    ((:hexadecimal :binary)
     (format nil "0x~4,'0x" register))
    (:signed
     (format nil "~d" (s16 register)))
    (:unsigned
     (format nil "~d" register))))

(defun combined-register (hi lo)
  ;; TODO: determine if we use arithemtic shift or logical shift.
  (+ (ash hi 8) lo))
(defun cpu-af (cpu)
  (combined-register (cpu-a cpu) (cpu-f cpu)))
(defun cpu-bc (cpu)
  (combined-register (cpu-b cpu) (cpu-c cpu)))
(defun cpu-de (cpu)
  (combined-register (cpu-d cpu) (cpu-e cpu)))
(defun cpu-hl (cpu)
  (combined-register (cpu-h cpu) (cpu-l cpu)))

(defun cpu-zero? (cpu)
  (not (zerop (logand (cpu-flag cpu) #b1000))))
(defun cpu-subtraction? (cpu)
  (not (zerop (logand (cpu-flag cpu) #b0100))))
(defun cpu-half-carry? (cpu)
  (not (zerop (logand (cpu-flag cpu) #b0010))))
(defun cpu-carry? (cpu)
  (not (zerop (logand (cpu-flag cpu) #b0001))))

#+nil
(command!
  (let ((*number-base* :unsigned))
    (update-cpu-visualization!
     (make-cpu :a 128 :b 255 :c 3 :d 4 :e 5 :f 6 :h 7 :l 8
	       :sp 9 :pc #x100 :flag #b1010))))

(defun read-rom-file! (filename)
  (with-open-file (stream filename :direction :input :element-type 'unsigned-byte)
    (let* ((length (file-length stream))
	   (array (make-array (list length) :element-type 'unsigned-byte :initial-element 0)))
      (loop for i below length
	    do (setf (aref array i) (read-byte stream)))
      array)))

(defparameter *rom* (read-rom-file! "gb_bios.bin"))
(defvar *memory* (make-array (list (1+ #xffff))
			     :element-type 'unsigned-byte
			     :initial-element 0))

(defun reset-memory! ()
  (setq *memory* (make-array (list (1+ #xffff))
			     :element-type 'unsigned-byte
			     :initial-element 0)))

(defun read-rom-file-into-memory! (filename &optional (start-addr 0))
  (with-open-file (stream filename :direction :input :element-type 'unsigned-byte)
    (let* ((length (file-length stream)))
      (loop for i below length
	    do (setf (aref *memory* (+ i start-addr)) (read-byte stream))))))


(defun hi-byte (u16)
  (truncate (logand #xFF00 u16) 256))
(defun lo-byte (u16)
  (logand #xFF u16))

(deftest test-hi-and-lo-byte
  (let ((n #xfefa))
    (checkeql n (combined-register (hi-byte n) (lo-byte n)))))

(defun flag (zero subtraction half-carry carry)
  (logior (* 8 zero)
	  (* 4 subtraction)
	  (* 2 half-carry)
	  carry))

(defun half-carry? (a b)
  (not (zerop (logand #x10 (+ (logand #xf a) (logand #xf b))))))

(defun instr-data (b1 &optional b2)
  (ecase b1
    (#x0c (alist :name :inc-c
		 :registers '(:c)
		 :flags '(:zero :subtraction :half-carry)
		 :description "Increment the contents of register C by 1."
		 :cycles (fn 1)))
    (#x0e (alist :name :ld-c-d8
		 :registers '(:c)
		 :flags '()
		 :description "Load the 8-bit immediate operand d8 into register C."
		 :cycles (fn 2)))
    (#x20 (alist :name :jr-nz-s8
		 :registers '(:pc)
		 :flags '()
		 :description "If the Z flag is 0, jump s8 steps from the current address stored in the program counter (PC).
If not, the instruction following the current JP instruction is executed (as usual)."
		 :cycles (fn (if (cpu-zero? %)
				 3
				 2))))
    (#x21 (alist :name :ld-hl-d16
		 :registers '(:hl :h :l)
		 :flags '()
		 :description "Load the 2 bytes of immediate data into register pair HL.

The first byte of immediate data is the lower byte (i.e., bits 0-7), 
and the second byte of immediate data is the higher byte (i.e., bits 8-15)."
		 :cycles (fn 3)))
    (#x31 (alist :name :ld-sp-imm
		 :registers '(:sp)
		 :flags '()
		 :description "Load the 2 bytes of immediate data into register pair SP.

The first byte of immediate data is the lower byte (i.e., bits 0-7), 
and the second byte of immediate data is the higher byte (i.e., bits 8-15)."
		 :cycles (fn 3)))
    (#x32 (alist :name :ld-hl-a
		 :registers '(:a :h :l :hl)
		 :flags '()
		 :description "Store the contents of register A into the memory location specified by register pair HL,
and simultaneously decrement the contents of HL."
		 :cycles (fn 2)))
    (#x3e (alist :name :ld-a-d8
		 :registers '(:a)
		 :flags '()
		 :description "Load the 8-bit immediate operand d8 into register A."
		 :cycles (fn 2)))
    (#xAF (alist :name :xor-a
		 :registers '(:a)
		 :flags '(:zero :subtraction :half-carry :carry)
		 :description "Take the logical exclusive-OR for each bit of the contents of register A and the contents of register A, 
and store the results in register A."
		 :cycles (fn 1)))
    (#xCB
     (ecase b2
       (#x7c (alist :name :bit-7-h
		    :registers '()
		    :flags '(:zero :subtraction :half-carry)
		    :description "Copy the complement of the contents of bit 7 in register H to the Z flag."
		    :cycles (fn 2)))))

    (#xE2 (alist :name :ld-@c-a
		 :registers '()
		 :flags '()
		 :description "Store the contents of register A in the internal RAM, port register, 
or mode register at the address in the range 0xFF00-0xFFFF specified by register C.

 - 0xFF00-0xFF7F: Port/Mode registers, control register, sound register
 - 0xFF80-0xFFFE: Working & Stack RAM (127 bytes)
 - 0xFFFF: Interrupt Enable Register"
		 :cycles (fn 2)))))

(defun next-instr-data (pc memory)
  (instr-data (aref memory pc) (aref memory (1+ pc))))

(defun disassemble-instr (pc memory)
  (let* ((instr (aref memory pc)))
    (ecase instr
      (#x0c (list :inc-c))
      (#x0e (list :ld-c-d8 (list :d8 (register8-text (aref memory (1+ pc))))))
      (#x20 (let ((s8 (s8 (aref memory (1+ pc)))))
	      (list :jr-nz-s8 (list :s8 s8) (list :addr (register16-text (+ pc s8 2))))))
      (#x21 (list :ld-hl-d16 (list :d16 (register16-text (combined-register (aref memory (+ 2 pc))
									    (aref memory (1+ pc)))))))
      (#x31 (list :ld-sp-imm (list :imm (register16-text
					 (combined-register (aref memory (+ pc 2)) (aref memory (1+ pc)))))))
      (#x32 (list :ld-hl-a))
      (#x3e (list :ld-a-d8 (list :d8) (register8-text (aref memory (1+ pc)))))
      (#xAF (list :xor-a))
      (#xCB
       (let* ((pc (1+ pc))
	      (instr (aref memory pc)))
	 (ecase instr
	   (#x7c (list :bit-7-h)))))

      (#xE2 (list :ld-@c-a)))))

(defun execute! ()
  (let* ((pc (cpu-pc *cpu*))
	 (instr (aref *memory* pc)))
    (ecase instr
      (#x0c
       ;; Inc C
       ;; 1 cycles
       (let* ((c (1+ (cpu-c *cpu*))))
	 (setf (cpu-flag *cpu*)
	       (flag (if (zerop c) 1 0)
		     0
		     (if (half-carry? (cpu-c *cpu*) 1) 1 0)
		     (if (cpu-carry? *cpu*) 1 0)))
	 (setf (cpu-c *cpu*) c))
       (incf (cpu-pc *cpu*) 1))
      (#x0e
       ;; Ld C, d8
       ;; 2 cycles
       (setf (cpu-c *cpu*) (aref *memory* (1+ pc)))
       (incf (cpu-pc *cpu*) 2))
      (#x20
       ;; JR NZ, s8
       (if (cpu-zero? *cpu*)
	   (let* ((s8 (s8 (aref *memory* (1+ pc)))))
	     ;; 3 cycles
	     (setf (cpu-pc *cpu*) (+ pc s8 2)))
	   ;; 2 cycles
	   (incf (cpu-pc *cpu*) 2)))
      (#x21
       ;; LD HL d16
       ;; 3 cycles
       (setf (cpu-l *cpu*) (aref *memory* (1+ pc))
	     (cpu-h *cpu*) (aref *memory* (+ 2 pc)))
       (incf (cpu-pc *cpu*) 3))
      (#x31
       ;; LD SP Immediate
       ;; 3 cycles
       (setf (cpu-sp *cpu*) (combined-register (aref *memory* (+ pc 2)) (aref *memory* (1+ pc))))
       (incf (cpu-pc *cpu*) 3))
      (#x32
       ;; LD HL- A
       ;; 2 cycles
       (setf (aref *memory* (cpu-hl *cpu*)) (cpu-a *cpu*))
       (let* ((hl (1- (cpu-hl *cpu*))))
	 (setf (cpu-h *cpu*) (hi-byte hl)
	       (cpu-l *cpu*) (lo-byte hl)))
       (incf (cpu-pc *cpu*) 1))
      (#x3e
       ;; LD a, d8
       ;; 2 cycles
       ;; see 0x0e
       (setf (cpu-a *cpu*) (aref *memory* (1+ pc)))
       (incf (cpu-pc *cpu*) 2))
      (#xAF
       ;; XOR A
       ;; 1 cycles
       (setf (cpu-a *cpu*) 0
	     (cpu-flag *cpu*) #b1000)
       (incf (cpu-pc *cpu*) 1))

      (#xCB
       (let* ((pc (1+ pc))
	      (instr (aref *memory* pc)))
	 (ecase instr
	   (#x7c
	    ;; Bit 7, H
	    ;; 2 cycles
	    (let* ((z (if (zerop (logand (cpu-h *cpu*) #x80))
			  1
			  0))
		   (s 0)
		   (h 1)
		   (c (if (cpu-carry? *cpu*) 1 0)))
	      (setf (cpu-flag *cpu*) (flag z s h c))
	      (incf (cpu-pc *cpu*) 2))))))

      (#xE2
       ;; LD (C) a
       ;; 2 cycles
       (setf (aref *memory* (+ #xff00 (cpu-c *cpu*)))
	     (cpu-a *cpu*))
       (incf (cpu-pc *cpu*) 1)))))
(run-tests!)

(register8-text 12)

(defparameter *cpu-visualization-pos* (g2 30 18))
(defparameter *cpu-visualization-previous-pos* (g2 30 2))

(defvar *cpu-visualization* (cpu-visualization "Current" (fn *cpu-visualization-pos*) (fn *cpu*)))
(defvar *cpu-visualization-previous* (cpu-visualization "Previous" (fn *cpu-visualization-previous-pos*) (fn *cpu-previous*)))

(defun reset! ()
  (setq *cpu* (cpu-initial))
  (setq *cpu-previous* (cpu-initial))
  (reset-memory!)
  (read-rom-file-into-memory! "gb_bios.bin")
  (load-text-texture! :disassembly :font "(Disassembly)")
  (load-text-texture! :disassembly-next
		      :font
		      (disassembly-text (cpu-pc *cpu*) *memory*))
  (update-cpu-visualization! *cpu-visualization*)
  (update-cpu-visualization! *cpu-visualization-previous*)

  (setq *cpu-visualization* (aset :colors
				  (print
				   (cpu-visualization-colors
				    ()
				    (next-instr-data (cpu-pc (funcall (aval :cpu-fn *cpu-visualization*))) *memory*)))
				  *cpu-visualization*))
  (setq *cpu-visualization-previous* (aset :colors () *cpu-visualization-previous*)))

(defhandler handle-initialize-cpu-visualization (event)
  (when (event-font-opened? event :font)
    (load-text-texture! :disassembly :font "(Disassembly)")
    (add-drawing! :disassembly (full-texture-drawing 1 :disassembly (g2 30 13)))

    (load-text-texture! :disassembly-next :font "(Next Disassembly)")
    (add-drawing! :disassembly-next (full-texture-drawing 1 :disassembly-next (g2 30 29)))

    (reset!)))

#+nil
(command! (reset!))

(defun add-mouse-pos-drawing! ()
  (add-drawing! :mouse-pos (full-texture-drawing 1 :mouse-pos (v2 0 0)))
  (add-drawing! :mouse-grid-highlight
		(drawing
		 0
		 (fn
		   (set-draw-color! 255 0 255 255)
		   (fill-rect! (g1 (truncate *mouse-x* *grid-size*))
			       (g1 (truncate *mouse-y* *grid-size*))
			       (g1 1)
			       (g1 1))))))

#+nil
(command!
  (if (aval :mouse-pos *drawings*)
      (progn
	(remove-drawing! :mouse-pos)
	(remove-drawing! :mouse-grid-highlight))
      (add-mouse-pos-drawing!)))

(defun disassembly-text (pc memory)
  (format nil "~a: ~A"
	  (register16-text pc)
	  (disassemble-instr pc memory)))

(defun cpu-visualization-colors (prev-instr-data instr-data)
  (let* ((modified (append (aval :registers prev-instr-data)
			   (aval :flags prev-instr-data)))
	 (next (append (aval :registers instr-data)
		       (aval :flags instr-data)))

	 (just-modified (set-difference modified next))
	 (just-next (set-difference next modified))
	 (both (intersection modified next)))
    (nconc
     (mapcar (fn (cons % (red))) just-modified)
     (mapcar (fn (cons % (green))) just-next)
     (mapcar (fn (cons % (yellow))) both))))

(defun handle-execute-button-clicked! ()
  (let ((prev (disassembly-text (cpu-pc *cpu*) *memory*))
	(prev-instr-data (next-instr-data (cpu-pc *cpu*) *memory*)))
    (setq *cpu-previous* (copy-cpu *cpu*))
    (setq *cpu-visualization-previous* (aset :colors (aval :colors *cpu-visualization*) *cpu-visualization-previous*))
    
    (execute!)
    (load-text-texture! :disassembly :font prev)
    (load-text-texture! :disassembly-next
			:font
			(disassembly-text (cpu-pc *cpu*) *memory*))
    (update-cpu-visualization! *cpu-visualization*)
    (update-cpu-visualization! *cpu-visualization-previous*)

    (let* ((instr-data (next-instr-data (cpu-pc *cpu*) *memory*)))
      (setq *cpu-visualization* (aset :colors (cpu-visualization-colors prev-instr-data instr-data) *cpu-visualization*)))))

;; TODO: add a remove-button!
(defun add-button! (button-id text pos layer font-id click-fn)
  (let* ((control-id (gensym)))
    (create-button! button-id
		    (make-instance 'button-spec :pos pos
						:bottom-layer layer
						:text text
						:text-drawing-id (gensym)
						:texture-id (gensym)
						:font-id font-id
						:control-id control-id))
    (register-handler! (gensym)
		       (fn (when (event-control-clicked? % control-id)
			     (funcall click-fn))))))

(defbutton execute "Execute!" (g2 30 30) 1 :font
  (handle-execute-button-clicked!))

(defbutton reset "Reset" (g2 35 30) 1 :font
  (reset!))

(defun mouse-pos-text ()
  (format nil "<~3,' d, ~3,' d><G~2,' d, G~2,' d>"
	  *mouse-x*
	  *mouse-y*
	  (truncate *mouse-x* *grid-size*)
	  (truncate *mouse-y* *grid-size*)))

(defhandler initialize-mouse-cursor-pos-text (event)
  (when (event-font-opened? event :font)
    (load-text-texture! :mouse-pos :font (mouse-pos-text))
    (add-mouse-pos-drawing!)))

;; show me some memory (memory being accessed?)
;; show description of the next instruction to execute.
;; a way to hide/show drawings would be nice
;; central definition for instructions (compile into an execute)
;; show me description of instruction (optionally?)
;; Join buttons and controls
