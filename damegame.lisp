;;;; damegame.lisp

(in-package #:damegame)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-keyword (symbol)
    (intern (symbol-name symbol) (find-package :keyword)))
  (defun symbolicate (&rest symbols)
    (intern (apply 'concatenate 'string (mapcar 'symbol-name symbols))))
  (defvar *tests* (make-hash-table))
  
  (defvar *commands* ())

  (defvar *running?* nil)
  (defvar *initialization-finished?* nil))

(defmacro when-let ((name condition-form) &body body)
  `(let* ((,name ,condition-form))
     (when ,name
       ,@body)))

(defmacro when-aval ((name key alist) &body body)
  `(when-let (,name (aval ,key ,alist))
     ,@body))

(defmacro command! (&body body)
  `(if *running?*
       (push (fn ,@body) *commands*)
       (progn ,@body)))

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
(defparameter *width* 1280 "The width of the window.")
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
(defvar *textures* (make-hash-table :test 'equal))

(defun event-font-opened (font-id)
  (alist :type :font-opened
	 :font-id font-id))

(defun load-font! (font-id path size)
  "Opens the font and stores it in *fonts*, closing any existing font."
  (let* ((existing-font (gethash font-id *fonts*)))
    ;; If another font with the same id is already open, close it first.
    (when existing-font
      (close-font! existing-font))
    ;; Open the font and add it to the *fonts* hash-table
    (setf (gethash font-id *fonts*) (open-font! path size))
    (notify-handlers! (event-font-opened font-id))))

(defun event-texture-loaded (texture-id)
  (alist :type :texture-loaded
	 :texture-id texture-id))
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
       (notify-handlers! (event-texture-loaded id))))))

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

(defun akeep (alist &rest keys)
  (remove-if-not (fn (member % keys))
		 alist
		 :key 'car))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun aremove (alist &rest keys)
    "Return a new alist with keys removed from alist."
    (remove-if (fn (member % keys))
	       alist
	       :key 'car))
  (defun aset (key value alist)
    "Return a new alist with the value associated with key added or replaced."
    (acons key value (aremove alist key))))

(defun akeys (alist)
  "Return a list of all keys in alist."
  (mapcar 'car alist))
(defun amerge (old &rest new-alists)
  "Return a new alist with the keys of old and new, where conflicts favor the newer alist."
  (if new-alists
      (let* ((new (first new-alists)))
	(apply 'amerge
	       ;; => ((:C . 3) (:B . 2) (:D . 5) (:A . 2) ((:E . 6) (:C . 4)))
	       (nconc (apply 'aremove old (akeys new))
		      new)
	       (rest new-alists)))
      old))
(defun amap (fn alist)
  "Apply fn to each key and value of alist returning a list of the results."
  (mapcar (fn (funcall fn (car %) (cdr %)))
	  alist))
(defun akey? (key alist)
  (member key alist :key 'car))

(defmacro asetq (id value alist-name)
  `(setq ,alist-name (aset ,id ,value ,alist-name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *resets* ()
    "A-list of variable-name, to (fn) which resets the variable-name.")
  (defvar *visualizations* ()))

(defmacro defreset-var (var-name reset-form &optional documentation)
  `(progn
     (defvar ,var-name nil ,documentation)
     (asetq ',var-name (lambda () ,reset-form) *resets*)))
(defmacro defvisualization (id visualization)
  (let* ((vis-name (gensym)))
    `(if *running?*
	 (command!
	   (let* ((,vis-name ,visualization))
	     (asetq ,id ,vis-name *visualizations*)
	     (register-visualization! ,id ,vis-name)))
	 (asetq ,id ,visualization *visualizations*))))
(defmacro undefvisualization (id &rest ignored)
  (declare (ignore ignored))
  `(if *running?*
       (command!
	 (remove-visuaulization! ,id)
	 (setq *visualizations* (aremove *visualizations* ,id)))
       (setq *visualizations* (aremove *visualizations* ,id))))

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

(defun fill-rect-drawing (layer color rect)
  (drawing layer
	   (fn
	     (set-draw-color! (r color) (g color) (b color) (a color))
	     (fill-rect! (x rect) (y rect) (w rect) (h rect)))))


(defun point-in-rect? (v2 r)
  "True if the given v2 point is inside of the rect top-left inclusive, bottom-right exclusive."
  (and (<= (x r) (x v2) (1- (+ (x r) (w r))))
       (<= (y r) (y v2) (1- (+ (y r) (h r))))))

(defvar *event-handlers* (make-hash-table :test 'equal)
  "A hash-table of runtime-created event-handlers.")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *compiled-event-handlers* (make-hash-table)
    "A hash-table of compile-time created event-handlers."))

(defun event-handler (event-matcher event-response)
  (cons event-matcher event-response))

(defun event-button-clicked (button-id)
  (alist :type :button-clicked
	 :button-id button-id))

(defun event-matches? (event-matcher event)
  (funcall event-matcher event))

(defun notify-handlers! (event)
  "Call the compiled & runtime event handlers with the given event."
  (let* ((notify (lambda (id event-handler)
		   (when (event-matches? (car event-handler) event)
		     (funcall (cdr event-handler) event id)))))
    (maphash notify *compiled-event-handlers*)
    (maphash notify *event-handlers*)))
(defun register-handler! (id event-handler)
  "Add/replace the handler in *event-handlers*"
  (setf (gethash id *event-handlers*) event-handler))
(defun remove-handler! (id)
  "Remove the handler from *event-handlers*"
  (remhash id *event-handlers*))
(defmacro defhandler (name (event) event-matcher-form &body body)
  "Add/replace the handler NAME in *compiled-event-handlers*"
  (let ((handler-id (gensym)))
    `(setf (gethash ',name *compiled-event-handlers*)
	   (event-handler
	    ,event-matcher-form
	    (lambda (,event ,handler-id)
	      (declare (ignorable ,event ,handler-id))
	      ,@body)))))
(defmacro undefhandler (name &rest rest)
  "Remove the handler NAME from *compiled-event-handlers*"
  (declare (ignore rest))
  `(remhash ',name *compiled-event-handlers*))

(defun draw-drawing! (drawing)
  (cond
    ((listp drawing)
     (ecase (aval :type drawing)
       (:texture
	(let* ((texture-id (aval :texture-id drawing))
	       (color (aval :color drawing))
	       (right-aligned? (aval :right-aligned? drawing))
	       (pos (aval :pos drawing)))
	  (when color
	    (set-texture-color! texture-id color))
	  (if right-aligned?
	      (draw-full-texture-id-right-aligned! texture-id pos)
	      (draw-full-texture-id! texture-id pos))))
       (:drawings
	(mapcar 'draw-drawing! (aval :drawings drawing)))))
    (t (funcall drawing))))

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
	  (amap (fn (button-handle-mouse-move! %)) *buttons*))
	 (event-mousedown
	  (when (eql :left (event-mousedown-button %))
	    (amap (fn (button-handle-mouse-down! %)) *buttons*)))
	 (event-mouseup
	  (when (eql :left (event-mouseup-button %))
	    (amap (fn (button-handle-mouse-up! %)) *buttons*))))))

  ;; Update based on time-step

  ;; Render to the screen
  (set-draw-color! 0 0 0 255)
  (clear!)
  (amap (fn (draw-drawing! (aval :fn %%))) *drawings*)
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

(defun event-initialization-finished ()
  (alist :type :initialization-finished))

(defvar *sdl-wrapper-initialized?* nil)
(defvar *system-initialized?* nil)
(defun main! ()
  "Entry point into the application. Recompiles the SDL-Wrapper, creates the window, 
and starts the update loop, then afterwards closes SDL and cleans up the application."
  (setq *running?* t)
  (unless *sdl-wrapper-initialized?*
    (recompile-sdl-wrapper-dll!)
    (setq *sdl-wrapper-initialized?* t))
  (unwind-protect 
       (progn
	 (setq *quit?* nil)
	 (clrhash *fonts*)
	 (clrhash *textures*)
	 (clrhash *event-handlers*)
	 (setq *drawings* ())
	 (setq *events* ())
	 (unless *system-initialized?*
	   (reset-vars!))

	 (start! *width* *height* *audio-frequency* *audio-channels*)
	 (load-font! :font "DroidSansMono.ttf" 16)
	 
	 (amap 'register-visualization! *visualizations*)
	 (amap (fn (initialize-visualization! %%)) *visualizations2*)
	 (notify-handlers! (event-initialization-finished))
	 (setq *initialization-finished?* t)

	 (unless *system-initialized?*
	   (reset!)
	   (setq *system-initialized?* t))

	 (main-loop!))
    (maphash (fn (close-font! %%)) *fonts*)
    (maphash (fn (free-texture! %%)) *textures*)
    (clrhash *fonts*)
    (clrhash *textures*)
    (when *tile-textures*
      (map nil 'free-texture! *tile-textures*)
      (setq *tile-textures* nil))
    (when *tile-map-texture*
      (free-texture! *tile-map-texture*)
      (setq *tile-map-texture* nil))
    (setq *initialization-finished?* nil
	  *running?* nil)
    (quit!)))

(defun event-matcher-update-visualization ()
  (fn (eql (aval :type %) :update-visualization)))
(defun event-matcher-font-opened (font-id)
  (fn (event-font-opened? % font-id)))
(defun event-matcher-initialization-finished ()
  'event-initialization-finished?)
(defun event-matcher-button-clicked (button-id)
  (fn
    (and (eql (aval :type %) :button-clicked)
	 (eql button-id (aval :button-id %)))))

(defun event-initialization-finished? (event)
  (eql (aval :type event) :initialization-finished))
(defun event-font-opened? (event font-id)
  "True if the event is an event-font-opened for the font-id"
  (and (eql (aval :type event) :font-opened)
       (eql font-id (aval :font-id event))))
(defun event-button-clicked? (event button-id)
  "True if event is button-id's button being clicked."
  (and (eql (aval :type event) :button-clicked)
       (eql button-id (aval :button-id event))))
(defun event-texture-loaded? (event texture-id)
  "True if event is texture-id's texture being created."
  (and (eql (aval :type event) :texture-loaded)
       (eql texture-id (aval :texture-id event))))

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

(defun register-one-off-handler! (event-matcher handle-fn)
  "Registers an event handler that will be removed once handled.
Only calls handle-fn if test-fn returns a truthy value.
Test-fn and handle-fn are both functions of event."
  (register-handler!
   (gensym)
   (event-handler
    event-matcher
    (lambda (event handler-id)
      (funcall handle-fn event)
      (remove-handler! handler-id)))))

(defun texture-rect (pos texture)
  "Returns a rect positioned at pos with the same dimensions as texture."
  (rect (x pos) (y pos) (texture-width texture) (texture-height texture)))

;; Creation: text, layer, pos, font-id,
(defun button (text layer pos font-id)
  "Button spec used for defbutton."
  (alist :text text
	 :layer layer
	 :pos pos
	 :font-id font-id))

(defvar *buttons* ())
(defun get-button (button-id)
  (aval button-id *buttons*))
(defun set-button! (button-id button)
  (asetq button-id button *buttons*))

;; Initialization: rect, loads texture-id, create drawing-id
(defun initialize-button! (button-id)
  (let* ((button (get-button button-id))
	 (texture-id (gensym))
	 (font-id (aval :font-id button))
	 (text (aval :text button))
	 (pos (aval :pos button))
	 (drawing-id (gensym))
	 (layer (aval :layer button)))
    (load-text-texture! texture-id font-id text)
    (let* ((texture (gethash texture-id *textures*))
	   (rect (rect (x pos) (y pos) (texture-width texture) (texture-height texture))))
      (set-button! button-id (amerge button (alist :rect rect
						   :texture-id texture-id
						   :drawing-id drawing-id)))
      (add-drawing! drawing-id
		    (drawing layer (fn (draw-button! button-id)))))))

(defun remove-button! (button-id)
  (let* ((button (get-button button-id))
	 (texture-id (aval :texture-id button))
	 (drawing-id (aval :drawing-id button))
	 (handler-id (aval :click-handler-id button)))
    (remove-drawing! drawing-id)
    (unload-texture! texture-id)
    (setq *buttons* (aremove *buttons* button-id))
    (when handler-id
      (remove-handler! handler-id))))

(defun button-color (button)
  (cond
    ((aval :pressed? button) (red))
    ((aval :hovered? button) (grey 128))
    (t (grey 60))))

(defhandler handle-intialize-buttons (event)
	    (event-matcher-initialization-finished)
  (amap (fn (initialize-button! %)) *buttons*))

;;; Lifetime of a button
;; Draw: rect, texture-id, pos, (hovered?, pressed?)
(defun draw-button! (button-id)
  (let* ((button (get-button button-id))
	 (texture-id (aval :texture-id button))
	 (pos (aval :pos button))
	 (rect (aval :rect button)))
    (set-color! (button-color button))
    (fill-rect! (x rect) (y rect) (w rect) (h rect))

    (draw-full-texture-id! texture-id pos)))

;; Watch for mouse-motion, mouse-down, and mouse-up
(defun button-handle-mouse-move! (button-id)
  "Process the effects of a mouse-move event on the button associated with button-id."
  (let* ((button (get-button button-id))
	 (rect (aval :rect button))
	 (hovered? (aval :hovered? button))
	 (in-rect? (point-in-rect? (v2 *mouse-x* *mouse-y*) rect))
	 (next-hovered?
	   (cond
	     ((and (not hovered?) in-rect?) t)
	     ((and hovered? (not in-rect?)) nil)
	     (t hovered?))))
    (set-button! button-id (aset :hovered? next-hovered? button))))

(defun button-handle-mouse-down! (button-id)
  "Process the effects of a left-mouse-button press on the button associated with button-id."
  (let* ((button (get-button button-id)))
    (when (aval :hovered? button)
      (set-button! button-id (aset :pressed? t button)))))
(defun button-handle-mouse-up! (button-id)
  "Process the effects on the button associated button-id of the left mouse button being released"
  (let* ((button (get-button button-id)))
    (when (and (aval :pressed? button) (aval :hovered? button))
      (notify-handlers! (event-button-clicked button-id)))
    ;; set pressed? to nil
    (set-button! button-id (aset :pressed? nil button))))

(defmacro defbutton (name button &body on-click)
  (let ((event (gensym))
	(button-name (gensym)))
    `(let* ((,button-name ,button))
       (defhandler ,(symbolicate 'handle- name '-clicked!) (,event)
		   (event-matcher-button-clicked ',name)
	 ,@on-click)
       (command!
	 (set-button! ',name ,button-name)
	 (when (gethash (aval :font-id ,button-name) *fonts*)
	   (initialize-button! ',name)))
       ',name)))
(defmacro undefbutton (name &rest args)
  (declare (ignore args))
  `(progn
     (undefhandler ,(symbolicate 'handle- name '-clicked!))
     (command! (remove-button! ',name))))


(defun unload-texture! (id)
  "Frees the texture and removes it from *textures*."
  (let* ((texture (gethash id *textures*)))
    (when texture
      (free-texture! texture)
      (remhash id *textures*))))

(defun texture-right-aligned (x texture-id)
  (let* ((texture (gethash texture-id *textures*)))
    (if texture
	(- x (texture-width texture))
	0)))

(defun texture-right-alignment (texture-id pos)
  (v2 (texture-right-aligned (x pos) texture-id) (y pos)))

(defun draw-full-texture-id-right-aligned! (texture-id pos)
  (draw-full-texture-id! texture-id (texture-right-alignment texture-id pos)))

(defstruct cpu
  (a 0 :type (unsigned-byte 8))
  (b 0 :type (unsigned-byte 8))
  (c 0 :type (unsigned-byte 8))
  (d 0 :type (unsigned-byte 8))
  (e 0 :type (unsigned-byte 8))
  (h 0 :type (unsigned-byte 8))
  (l 0 :type (unsigned-byte 8))
  (sp 0 :type (unsigned-byte 16))
  (pc 0 :type (unsigned-byte 16))
  (zero? nil :type boolean)
  (half-carry? nil :type boolean)
  (carry? nil :type boolean)
  (subtraction? nil :type boolean)
  (f-lo 0 :type (unsigned-byte 8))
  (ime? nil :type boolean))

(defun g1 (value) (* value *grid-size*))

(defun cpu-f-hi (cpu)
  (+ (if (cpu-zero? cpu) #b1000 0)
     (if (cpu-subtraction? cpu) #b0100 0)
     (if (cpu-half-carry? cpu) #b0010 0)
     (if (cpu-carry? cpu) #b0001 0)))

(defun cpu-f (cpu)
  (+ (ash (cpu-f-hi cpu) 4)
     (cpu-f-lo cpu)))

(defun set-cpu-f! (cpu f)
  (setf (cpu-f-lo cpu) (logand #xF f)
	(cpu-zero? cpu) (bit-set? 7 f)
	(cpu-subtraction? cpu) (bit-set? 6 f)
	(cpu-half-carry? cpu) (bit-set? 5 f)
	(cpu-carry? cpu) (bit-set? 4 f)))
(defsetf cpu-f set-cpu-f!)

(defun cpu-initial ()
  (make-cpu :a 0 :b 0 :c 0 :d 0 :e 0 :f-lo 0 :h 0 :l 0 :sp 0 :pc 0
	    :zero? nil :half-carry? nil :subtraction? nil :ime? nil))

(defreset-var *cpus* (list (cpu-initial)))
(defun cpu-current ()
  (first *cpus*))
(defun cpu-previous ()
  (second *cpus*))

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


(defun memory-visualization (title pos start-addr-ref byte-text-fn)
  (let ((ids (make-array (list *memory-visualization-byte-count*))))
    (loop for i below (length ids)
	  do (setf (aref ids i) (gensym)))
    (alist :pos pos
	   :start-addr-ref start-addr-ref
	   :texture-ids ids
	   :title title
	   :title-texture-id (gensym)
	   :byte-text-fn byte-text-fn)))
(defparameter *memory-visualization-byte-count* 24)

(defun draw-memory-visualization! (cpu memory-visualization-id)
  (let* ((memory-visualization (get-memory-visualization memory-visualization-id))
	 (ids (aval :texture-ids memory-visualization))
	 (pos (aval :pos memory-visualization))
	 (start-addr (funcall (aval :start-addr-ref memory-visualization))))
    (draw-full-texture-id! (aval :title-texture-id memory-visualization) pos)
    (loop for i below (length ids)
	  do
	     (let ((addr (+ i start-addr))
		   (texture-id (aref ids i)))
	       (set-texture-color! texture-id 
				   (cond
				     ((= addr (cpu-pc cpu)) (green))
				     ((= addr (cpu-sp cpu)) (blue))
				     ((= addr (cpu-hl cpu)) (yellow))
				     (t (white))))
	       (draw-full-texture-id! texture-id (v+ pos (g2 0 (1+ i))))))))
(defun update-memory-visualization! (memory memory-visualization)
  (let* ((ids (aval :texture-ids memory-visualization))
	 (byte-text-fn (aval :byte-text-fn memory-visualization))
	 (start-addr (funcall (aval :start-addr-ref memory-visualization))))
    (loop for i below (length ids)
	  do (load-text-texture! (aref ids i)
				 :font
				 (format nil "~A: ~A"
					 (hex16-text (+ i start-addr))
					 (funcall byte-text-fn (mem-byte (+ i start-addr) memory)))))))

(defun start-addr (focus-addr)
  (min (max (- focus-addr (/ *memory-visualization-byte-count* 2)) 0) (- #xffff *memory-visualization-byte-count*)))

(defvar *memory-visualizations* ())
(defun get-memory-visualization (id)
  (aval id *memory-visualizations*))
(defun set-memory-visualization! (id value)
  (asetq id value *memory-visualizations*))

(defun remove-memory-visualization! (id)
  (let* ((vis (get-memory-visualization id)))
    (when vis
      (let* ((drawing-id (aval :drawing-id vis)))
	(remove-drawing! drawing-id)
	(setq *memory-visualizations* (aremove *memory-visualizations* id))))))

(defmacro defmemory-visualization (name memory-visualization)
  `(progn
     (command!
       (set-memory-visualization! ',name ,memory-visualization)
       (when (gethash :font *fonts*)
	 (initialize-memory-visualization! ',name (get-memory-visualization ',name))))
     ',name))
(defmacro undefmemory-visualization (name &rest args)
  (declare (ignore args))
  `(command! (remove-memory-visualization! ',name)))

(defun draw-memory-visualizations! ()
  (amap (fn (draw-memory-visualization! (cpu-current) %)) *memory-visualizations*))

(defun initialize-memory-visualization! (id memory-visualization)
  (let* ((drawing-id (gensym)))
    (set-memory-visualization! id (aset :drawing-id drawing-id memory-visualization))
    (update-memory-visualization! *memory* memory-visualization)
    ;;(add-drawing! drawing-id (drawing 1 (fn (draw-memory-visualization! (cpu-current) id))))
    (load-text-texture! (aval :title-texture-id memory-visualization) :font
			(aval :title memory-visualization))))

(defmemory-visualization pc (memory-visualization "PC" (g2 1 3) (fn (start-addr (cpu-pc (cpu-current))))
						  'register8-text))
(defmemory-visualization stack (memory-visualization "Stack" (g2 11 3) (fn (start-addr (cpu-sp (cpu-current))))
						     'register8-text))
(defmemory-visualization hl (memory-visualization "HL" (g2 21 3) (fn (start-addr (cpu-hl (cpu-current))))
						  'register8-text))

(defvar *main-panel* :memory)
(defun draw-memory-visualizations? ()
  (eql *main-panel* :memory))

(defhandler handle-initialize-memory-visualization (event)
	    (event-matcher-initialization-finished)
  (amap 'initialize-memory-visualization! *memory-visualizations*)
  (add-drawing! :memory-visualizations (drawing 1 (fn (when (draw-memory-visualizations?)
							(draw-memory-visualizations!))))))

(defvar *cpu-visualizations* ())
(defun get-cpu-visualization (id)
  (aval id *cpu-visualizations*))
(defun set-cpu-visualization! (id value)
  (asetq id value *cpu-visualizations*))

(defmacro defcpu-visualization (name cpu-visualization)
  `(progn
     (command!
       (set-cpu-visualization! ',name ,cpu-visualization)
       (when (gethash :font *fonts*)
	 (initialize-cpu-visualization! ',name (get-cpu-visualization ',name))))
     ',name))
(defmacro undefcpu-visualization (name &rest args)
  (declare (ignore args))
  `(command! (remove-cpu-visualization! ',name)))

(defun remove-cpu-visualization! (id)
  (let* ((vis (get-cpu-visualization id)))
    (when vis
      ;; TODO
      )))

(defun draw-cpu-visualization! (id)
  (let* ((cpu-visualization (get-cpu-visualization id))
	 (cpu-pos (aval :pos cpu-visualization)))
    (set-color! (white))
    (draw-rect! (- (x cpu-pos) (/ *grid-size* 2))
		(- (y cpu-pos) (/ *grid-size* 2))
		(g1 19)
		(g1 11))
    (draw-full-texture-id! (aval :title-texture-id cpu-visualization)
			   (v+ cpu-pos (v2 0 (truncate (g1 -1.5)))))
    (let* ((cpu (funcall (aval :cpu-fn cpu-visualization)))
	   (cpu-colors (cpu-visualization-colors cpu-visualization))
	   
	   (left-column (cpu-state-left-column cpu-pos))
	   (right-column (cpu-state-right-column cpu-pos))
	   (y (y cpu-pos)))
      (draw-full-texture-id-right-aligned! :zero (v2 left-column y))
      (draw-full-texture-id-right-aligned! :subtraction (v2 right-column y))
      (let ((texture-id (flag-state-texture-id (cpu-zero? cpu))))
	(set-texture-color! texture-id (aval :zero? cpu-colors (white)))
	(draw-full-texture-id! texture-id (v2 left-column y)))
      (let ((texture-id (flag-state-texture-id (cpu-subtraction? cpu))))
	(set-texture-color! texture-id (aval :subtraction? cpu-colors (white)))
	(draw-full-texture-id! texture-id (v2 right-column y)))
      
      (incf y *grid-size*)
      (draw-full-texture-id-right-aligned! :half-carry (v2 left-column y))
      (draw-full-texture-id-right-aligned! :carry (v2 right-column y))
      (let ((texture-id (flag-state-texture-id (cpu-half-carry? cpu))))
	(set-texture-color! texture-id (aval :half-carry? cpu-colors (white)))
	(draw-full-texture-id! texture-id (v2 left-column y)))
      (let ((texture-id (flag-state-texture-id (cpu-carry? cpu))))
	(set-texture-color! texture-id (aval :carry? cpu-colors (white)))
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
      (let ((column (+ (g1 9) (x cpu-pos))))
	(incf y (g1 1))
	(draw-full-texture-id-right-aligned! :stack-pointer (v2 column y))
	(set-texture-color! (aval :sp cpu-visualization) (aval :sp cpu-colors (white)))
	(draw-full-texture-id! (aval :sp cpu-visualization) (v2 column y))

	(draw-full-texture-id-right-aligned! :ime (v2 (+ (g1 18) (x cpu-pos)) y))

	(incf y (g1 1))
	(draw-full-texture-id-right-aligned! :program-counter (v2 column y))
	(set-texture-color! (aval :pc cpu-visualization) (aval :pc cpu-colors (white)))
	(draw-full-texture-id! (aval :pc cpu-visualization) (v2 column y))

	(let ((texture-id (flag-state-texture-id (cpu-ime? cpu))))
	  (set-texture-color! texture-id (aval :ime? cpu-colors (white)))
	  (draw-full-texture-id! texture-id (v2 (+ (g1 15) (x cpu-pos)) y)))))))

(defun initialize-cpu-visualization! (id vis)
  (let ((drawing-id (gensym)))
    (set-cpu-visualization! id (aset :drawing-id drawing-id vis))
    (load-text-texture! (aval :title-texture-id vis) :font (aval :title vis))
    (update-cpu-visualization! vis)
    (add-drawing! drawing-id (drawing *cpu-state-layer* (fn (draw-cpu-visualization! id))))))

(defun cpu-visualization (title pos cpu-fn previous-cpu-fn)
  (alist
   :title title
   :cpu-fn cpu-fn
   :previous-cpu-fn previous-cpu-fn
   :title-texture-id (gensym)
   :pos pos
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

(defcpu-visualization current
    (cpu-visualization "Current" (g2 32 18)
		       'cpu-current
		       (fn (or (cpu-previous) (cpu-initial)))))
(defcpu-visualization previous
    (cpu-visualization "Previous" (g2 32 2)
		       (fn (or (cpu-previous) (cpu-initial)))
		       (fn (or (third *cpus*) (cpu-initial)))))

(defhandler load-cpu-visualization (event)
	    (event-matcher-initialization-finished)
  (load-text-texture! :no :font "[No]")
  (load-text-texture! :yes :font "[Yes]")
  (load-text-texture! :zero :font "Zero? ")
  (load-text-texture! :ime :font "IME? ")
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

  (amap 'initialize-cpu-visualization! *cpu-visualizations*))

(defun flag-state-texture-id (set?)
  (if set? :yes :no))

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

(defun bin8-text (num)
  (format nil "0b~8,'0b" num))
(defun hex8-text (num)
  (format nil "0x~2,'0x" num))
(defun s8-text (num)
  (format nil "~d" (s8 num)))
(defun u8-text (num)
  (format nil "~d" num))
(defun hex16-text (num)
  (format nil "0x~4,'0x" num))
(defun register8-text (register)
  (ecase *number-base*
    (:hexadecimal
     (hex8-text register))
    (:signed
     (s8-text register))
    (:unsigned
     (u8-text register))
    (:binary
     (bin8-text register))))
(defun register16-text (register)
  (ecase *number-base*
    ((:hexadecimal :binary)
     (hex16-text register))
    (:signed
     (format nil "~d" (s16 register)))
    (:unsigned
     (format nil "~d" register))))

(defun combined-register (hi lo)
  (+ (ash hi 8) lo))
(defun cpu-af (cpu)
  (combined-register (cpu-a cpu) (cpu-f cpu)))
(defun cpu-bc (cpu)
  (combined-register (cpu-b cpu) (cpu-c cpu)))
(defun cpu-de (cpu)
  (combined-register (cpu-d cpu) (cpu-e cpu)))
(defun cpu-hl (cpu)
  (combined-register (cpu-h cpu) (cpu-l cpu)))

(defun read-rom-file! (filename)
  (with-open-file (stream filename :direction :input :element-type 'unsigned-byte)
    (let* ((length (file-length stream))
	   (array (make-array (list length) :element-type 'unsigned-byte :initial-element 0)))
      (loop for i below length
	    do (setf (aref array i) (read-byte stream)))
      array)))

(defun cart-title (cart-rom-header)
  (map 'string 'code-char (remove 0 (subseq cart-rom-header #x134 #x144))))
(defun cart-type-code (cart-rom-header)
  (aref cart-rom-header #x147))
(defun cart-type (cart-type-code)
  (ecase cart-type-code
    (#x00 :ROM-only)
    (#x01 :MBC1)
    (#x02 :MBC1+RAM)
    (#x03 :MBC1+RAM+BATTERY)
    (#x05 :MBC2)
    (#x06 :MBC2+BATTERY)
    (#x08 :ROM+RAM)
    (#x09 :ROM+RAM+BATTERY)
    (#x0B :MMM01)
    (#x0C :MMM01+RAM)
    (#x0D :MMM01+RAM+BATTERY)
    (#x0F :MBC3+TIMER+BATTERY)
    (#x10 :MBC3+TIMER+RAM+BATTERY)
    (#x11 :MBC3)
    (#x12 :MBC3+RAM)
    (#x13 :MBC3+RAM+BATTERY)
    (#x19 :MBC5)
    (#x1A :MBC5+RAM)
    (#x1B :MBC5+RAM+BATTERY)
    (#x1C :MBC5+RUMBLE)
    (#x1D :MBC5+RUMBLE+RAM)
    (#x1E :MBC5+RUMBLE+RAM+BATTERY)
    (#x20 :MBC6)
    (#x22 :MBC7+SENSOR+RUMBLE+RAM+BATTERY)
    (#xFC :POCKET-CAMERA)
    (#xFD :BANDAI-TAMA5)
    (#xFE :HuC3)
    (#xFF :HuC1+RAM+BATTERY)))


(defun cart-rom-size-code (cart-rom-header)
  (aref cart-rom-header #x148))
(defun cart-rom-size (cart-rom-size-code)
  (ecase cart-rom-size-code
    (#x00 (list (list 32 :KB) 2))
    (#x01 (list (list 64 :KB) 4))
    (#x02 (list (list 128 :KB) 8))
    (#x03 (list (list 256 :KB) 16))
    (#x04 (list (list 512 :KB) 32))
    (#x05 (list (list 1 :MB) 64))
    (#x06 (list (list 2 :MB) 128))
    (#x07 (list (list 4 :MB) 256))
    (#x08 (list (list 8 :MB) 512))
    (#x52 (list (list 1.1 :MB) 72))
    (#x53 (list (list 1.2 :MB) 80))
    (#x54 (list (list 1.5 :MB) 96))))

(defun cart-ram-size-code (cart-rom-header)
  (aref cart-rom-header #x149))

(defun cart-ram-size (cart-ram-size-code)
  (ecase cart-ram-size-code
    (#x00 '((0 :kb) (0 :banks)))
    (#x01 '((2 :kb) (1 :banks)))
    (#x02 '((8 :kb) (1 :banks)))
    (#x03 '((32 :kb) (4 :banks)))
    (#x04 '((128 :kb) (16 :banks)))
    (#x05 '((64 :kb) (8 :banks)))))

(defun cart-version-number (cart-rom-header)
  (aref cart-rom-header #x14c))

(defun bios-rom-enabled? (memory)
  (zerop (aref memory #xff50)))

(defreset-var *memory* (make-array (list (1+ #xffff))
				   :element-type '(unsigned-byte 8)
				   :initial-element 0))
(defreset-var *ram-enabled?* nil)
(defreset-var *rom-bank* 1)
(defreset-var *bios-rom* (read-rom-file! "gb_bios.bin"))
(defreset-var *cart-rom* (read-rom-file! *cart-filename*))

(defun mem-write! (addr byte &optional (memory *memory*))
  (cond
    ((<= 0 addr #x1fff) (setq *ram-enabled?* (= #xA (logand #xF byte))))
    ((<= 2000 addr #x3fff)
     (setq *rom-bank* (max 1 (logand #b11111 byte)))
     (copy-memory-bank-into-memory!
      (memory-bank *cart-rom* *rom-bank*)
      :start-addr #x4000))
    (t (setf (aref memory addr) byte))))
(defun mem-byte (addr &optional (memory *memory*))
  (if (and (bios-rom-enabled? memory)
	   (< addr #x100))
      (aref *bios-rom* addr)
      (aref memory addr)))

(defun copy-memory-bank-into-memory! (memory-bank &key (start-addr 0))
  (loop for i below #x4000
	do (setf (aref *memory* (+ i start-addr)) (aref memory-bank i))))

(defun read-rom-file-into-memory! (filename &key (start-addr 0) (max-length #x4000))
  (with-open-file (stream filename :direction :input :element-type 'unsigned-byte)
    (let* ((length (file-length stream))
	   (size (min length max-length)))
      (loop for i below size
	    do (setf (aref *memory* (+ i start-addr)) (read-byte stream))))))
(defparameter *cart-filename* "Legend of Zelda, The - Link's Awakening (USA, Europe).gb")

(defun hi-byte (u16)
  (truncate (logand #xFF00 u16) 256))
(defun lo-byte (u16)
  (logand #xFF u16))

(deftest test-hi-and-lo-byte
  (let ((n #xfefa))
    (checkeql n (combined-register (hi-byte n) (lo-byte n)))))

(defun long-instr? (byte1)
  (eql byte1 #xcb))

(defun next-instr (cpu memory)
  (let* ((pc (cpu-pc cpu))
	 (byte (mem-byte pc memory)))
    (if (long-instr? byte)
	(let* ((byte2 (mem-byte (1+ pc) memory)))
	  (aval byte2 *long-instrs*))
	(aval byte *instrs*))))

(defun disassemble-instr-at-pc (pc cpu memory)
  (let ((cpu2 (copy-cpu cpu)))
    (setf (cpu-pc cpu2) pc)
    (disassemble-instr cpu2 memory)))

(defun disassemble-instr (cpu memory)
  (let* ((pc (cpu-pc cpu))
	 (byte (mem-byte pc memory)))
    (if (long-instr? byte)
	(let* ((byte2 (mem-byte (1+ pc) memory))
	       (instr (aval byte2 *long-instrs*)))
	  (if instr
	      (list* (cons :name (aval :name instr)) (funcall (aval :disassemble-instr instr) cpu memory))
	      (alist :name '(:unknown :bytes) :bytes (register16-text (combined-register byte byte2)))))
	(let* ((instr (aval byte *instrs*)))
	  (if instr
	      (list* (cons :name (aval :name instr)) (funcall (aval :disassemble-instr instr) cpu memory))
	      (alist :name '(:unknown :bytes) :bytes (hex8-text byte)))))))

(defvar *instruction-description-ids* (loop for i below 8 collecting (gensym)))

;; Variables that get reset:
;;   Variables that are part of the system
;;   variables that track the behavior of the system.

(defun reset-vars! ()
  (amap (fn (set % (funcall %%))) *resets*))

(defun reset! ()
  (reset-vars!)
  (copy-memory-bank-into-memory! (memory-bank *cart-rom* 0) :start-addr 0)
  (copy-memory-bank-into-memory! (memory-bank *cart-rom* *rom-bank*) :start-addr #x4000)

  (update-visualizations!)

  (loop for i from 0 below (length *lcd-pixel-buffer*)
	do (setf (aref *lcd-pixel-buffer* i) 255))
  (replace-pixel-buffer! (gethash :lcd *textures*) *lcd-pixel-buffer*))

(defreset-var *pc-path* (make-array (* 512 1024) :adjustable t :fill-pointer 0))
(defreset-var *memory-updates* ())

(defun step-cpus! ()
  (setq *cpus* (list (cpu-current) (copy-cpu (cpu-current)) (cpu-previous)))
  (vector-push-extend (cpu-pc (cpu-current)) *pc-path*)
  (when-aval (memory :memory (instr-effects (cpu-current) *memory*))
    (push (cons (cpu-pc (cpu-current)) memory) *memory-updates*))
  (execute! (cpu-current) *memory*))

(defun handle-execute-button-clicked! ()
  (step-cpus!)
  (update-visualizations!))

(defun split-lines (text)
  (word-wrap (remove-newlines text) 56))

(defun initialize-description! (description)
  (let* ((lines (split-lines description)))
    (loop for id in *instruction-description-ids*
	  do (remove-drawing! id))
    (let ((pos (g2 1 30)))
      (loop for id in *instruction-description-ids*
	    for i from 0
	    for line in lines do
	      (load-text-texture! id :font line)
	      (add-drawing! id (full-texture-drawing 1 id (v+ pos (g2 0 i)))))

      (add-drawing! :instruction-description-box
		    (drawing 1 (fn
				 (set-color! (white))
				 (draw-rect! (truncate (g1 .5))
					     (truncate (g1 29.5))
					     (g1 29)
					     (g1 (1+ (length lines))))))))))

(defvisualization :instruction-description
    (alist :initialization
	   (fn (initialize-description! (aval :description (next-instr (cpu-current) *memory*))))
	   :update
	   (fn (initialize-description! (aval :description (next-instr (cpu-current) *memory*))))))

(defvisualization :next-disassembly
    (alist :initialization
	   (fn
	     (load-text-texture! :disassembly-next :font (disassembly-text (cpu-current) *memory*))
	     (add-drawing! :disassembly-next (full-texture-drawing 1 :disassembly-next (g2 32 29))))

	   :update
	   (fn (load-text-texture! :disassembly-next :font (disassembly-text (cpu-current) *memory*)))))

(defvisualization :disassembly
    (alist :initialization
	   (fn
	     (load-text-texture! :disassembly :font "(Disassembly)")
	     (add-drawing! :disassembly (full-texture-drawing 1 :disassembly (g2 32 13))))
	   :update
	   (fn
	     (let* ((cpu-previous (cpu-previous)))
	       (load-text-texture! :disassembly :font
				   (if cpu-previous
				       (disassembly-text cpu-previous *memory*)
				       "(Disassembly)"))))))

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

(defun disassembly-text (cpu memory)
  (format nil "~a: ~A"
	  (register16-text (cpu-pc cpu))
	  (disassemble-instr cpu memory)))
(defparameter *register-keys*
  '(:a :b :c :d :e :f :h :l :af :bc :de :hl :sp :pc))
(defparameter *flag-keys*
  '(:zero? :subtraction? :half-carry? :carry?))
(defparameter *register8-keys* '(:a :b :c :d :e :f :h :l))
(defparameter *register16-keys* '(:af :bc :de :hl :sp :pc))

(defun instr-affected-keys (instr-effects)
  (let* ((keys (akeys instr-effects)))
    (if (aval :jump? instr-effects)
	(union '(:pc) keys)
	(remove :pc keys))))

(defun cpu-visualization-colors (cpu-visualization)
  (let* ((cpu (funcall (aval :cpu-fn cpu-visualization)))
	 (cpu-previous (funcall (aval :previous-cpu-fn cpu-visualization)))

	 (prev-instr-effects (when cpu-previous (instr-effects cpu-previous *memory*)))
	 (instr-effects (when cpu (instr-effects cpu *memory*)))

	 (modified (instr-affected-keys prev-instr-effects))
	 (next (instr-affected-keys instr-effects))

	 (just-modified (set-difference modified next))
	 (just-next (set-difference next modified))
	 (both (intersection modified next)))
    (nconc
     (mapcar (fn (cons % (red))) just-modified)
     (mapcar (fn (cons % (green))) just-next)
     (mapcar (fn (cons % (yellow))) both))))

(defun memory-name (addr)
  (cond
    ((<= addr #x3fff) :rom-bank0)
    ((<= addr #x7fff) :rom-bank1)
    ((<= addr #x9fff) :vram)
    ((<= addr #xbfff) :external-ram)
    ((<= addr #xCFFF) :work-ram-bank0)
    ((<= addr #xdfff) :work-ram-bank1)
    ((<= addr #xfdff) :unused)
    ((<= addr #xfe9f) :oam)
    ((<= addr #xfeff) :unused)
    ((= addr #xff00) :controller)
    ((= addr #xff01) :serial-transfer-data)
    ((= addr #xff02) :serial-transfer-control)
    ((= addr #xff03) :unknown)
    ((= addr #xff04) :divider)
    ((= addr #xff05) :timer-counter)
    ((= addr #xff06) :timer-modulo)
    ((= addr #xff07) :timer-control)
    ((= addr #xff07) :timer-control)
    ((< addr #xff10) :unknown)
    ((<= addr #xff26) :sound)
    ((< addr #xff30) :unknown)
    ((<= addr #xff3f) :waveform-ram)
    ((= addr #xff40) :lcd-control)
    ((= addr #xff41) :lcd-status)
    ((= addr #xff42) :scroll-y)
    ((= addr #xff43) :scroll-x)
    ((= addr #xff44) :lcdc-y-coordinate)
    ((= addr #xff45) :lcdc-y-compare)
    ((= addr #xff46) :dma-start-address)
    ((= addr #xff47) :bg-palette-data)
    ((= addr #xff48) :obj0-palette-data)
    ((= addr #xff49) :obj1-palette-data)
    ((= addr #xff4a) :window-y)
    ((= addr #xff4b) :window-x)
    ((< addr #xff50) :unknown)
    ((= addr #xff50) :boot-rom-disable)
    ((< addr #xff80) :unknown)
    ((< addr #xfffe) :high-ram)
    ((= addr #xffff) :interrupt-enable)))

(defun updated-memory-text (memory)
  (format nil "Memory: ~A" (if memory
			       (let* ((binding (first memory)))
				 (format nil "~A (~A): ~A"
					 (hex16-text (first binding))
					 (memory-name (first binding))
					 (register8-text (second binding))))
			       "-")))

(defun update-visualizations! ()
  (amap (fn (update-memory-visualization! *memory* %%)) *memory-visualizations*)
  (amap (fn (update-cpu-visualization! %%)) *cpu-visualizations*)

  (update-lcd-visualizations!)
  ;;(replace-pixel-buffer! (gethash :lcd *textures*) *lcd-pixel-buffer*)
  (notify-handlers! (alist :type :update-visualization)))

(defbutton execute (button "Execute!" 1 (g2 32 30) :font)
  (handle-execute-button-clicked!))

(defbutton reset (button "Reset" 1  (g2 32 31) :font)
  (reset!))

(defbutton refresh (button "Refresh" 1 (g2 32 32) :font)
  (update-visualizations!))

(defbutton continue (button "Continue!" 1 (g2 37 30) :font)
  (step-cpus!)
  (loop until (break? (cpu-current) *memory*)
	do (step-cpus!))
  (update-visualizations!))

(let* ((top 34))
  (defbutton hex (button "Hex" 1 (g2 32 top) :font)
    (setq *number-base* :hexadecimal)
    (update-visualizations!))
  (defbutton bin (button "Bin" 1 (g2 34 top) :font)
    (setq *number-base* :binary)
    (update-visualizations!))
  (defbutton signed (button "10s" 1 (g2 32 (1+ top)) :font)
    (setq *number-base* :signed)
    (update-visualizations!))
  (defbutton unsigned (button "10u" 1 (g2 34 (1+ top)) :font)
    (setq *number-base* :unsigned)
    (update-visualizations!)))

(defun mouse-pos-text ()
  (format nil "<~3,' d, ~3,' d><G~2,' d, G~2,' d>"
	  *mouse-x*
	  *mouse-y*
	  (truncate *mouse-x* *grid-size*)
	  (truncate *mouse-y* *grid-size*)))

(defhandler initialize-mouse-cursor-pos-text (event)
	    (event-matcher-initialization-finished)
  (load-text-texture! :mouse-pos :font (mouse-pos-text))
  (add-mouse-pos-drawing!))


(defvar *instrs* ())
(defvar *long-instrs* ())

(defparameter *cpu-name* 'cpu)
(defparameter *memory-name* 'memory)
(defparameter *pc-name* 'pc)
(defparameter *imm8-name* 'imm8)
(defparameter *imm16-name* 'imm16)
(defparameter *s8-name* 's8)
(defparameter *addr-name* 'addr)
(defparameter *data-name* 'data)
(defparameter *result-name* 'result)
(defparameter *a-name* 'a)
(defparameter *carry-bit-name* 'cy)

(defun compile-instr-effects (bindings effects)
  `(lambda (,*cpu-name* ,*memory-name*)
     (declare (ignorable ,*cpu-name* ,*memory-name*))
     (let* ((,*pc-name* (cpu-pc ,*cpu-name*)))
       (declare (ignorable ,*pc-name*))
       (let* ,bindings
	 (alist ,@(apply 'nconc (amap (fn (list % %%)) (aremove effects :memory)))
		,@(let ((memory (aval :memory effects)))
		    (list :memory (cons 'list (mapcar (fn (cons 'list %)) memory)))))))))

(defun compile-disassemble-instr (bindings disassembly)
  `(lambda (,*cpu-name* ,*memory-name*)
     (declare (ignorable ,*cpu-name* ,*memory-name*))
     (let* ((,*pc-name* (cpu-pc ,*cpu-name*)))
       (declare (ignorable ,*pc-name*))
       (let* ,bindings
	 (declare (ignorable ,@(mapcar 'first bindings)))
	 (alist ,@(apply 'append (amap (fn (list % %%)) disassembly)))))))

(defun compile-execute-next-instr ()
  `(defun execute-next-instr! (,*cpu-name* ,*memory-name*)
     (let ((,*pc-name* (cpu-pc ,*cpu-name*)))
       (ecase (mem-byte ,*pc-name* ,*memory-name*)
	 ,@(amap (fn (list % (compile-instr-spec-for-execute %%))) *instrs*)
	 (#xcb
	  ;; TODO: optimize pc-access
	  (ecase (mem-byte (1+ ,*pc-name*) ,*memory-name*)
	    ,@(amap (fn (list % (compile-instr-spec-for-execute %%))) *long-instrs*)))))))

(defun cpu-register-accessor-name (register-key)
  (ecase register-key
    (:a 'cpu-a)
    (:b 'cpu-b)
    (:c 'cpu-c)
    (:d 'cpu-d)
    (:e 'cpu-e)
    (:f 'cpu-f)
    (:h 'cpu-h)
    (:l 'cpu-l)
    (:sp 'cpu-sp)
    (:pc 'cpu-pc)
    (:af 'cpu-af)
    (:bc 'cpu-bc)
    (:de 'cpu-de)
    (:hl 'cpu-hl)
    (:zero? 'cpu-zero?)
    (:half-carry? 'cpu-half-carry?)
    (:carry? 'cpu-carry?)
    (:subtraction? 'cpu-subtraction?)))

(defun compile-register-set (key instr-spec)
  (when (akey? key instr-spec)
    `((,(cpu-register-accessor-name key) ,*cpu-name*) ,(aval key instr-spec))))

(defun compile-register-sets (instr-spec)
  (apply 'nconc
	 (mapcar (fn (compile-register-set % instr-spec))
		 '(:a :b :c :d :e :f :h :l :sp :pc))))

(defun compile-combined-register-set (instr-spec combined-key lo-key hi-key)
  (let ((combined-value (aval combined-key instr-spec))
	(combined-name (gensym)))
    (when combined-value
      `(let ((,combined-name ,combined-value))
	 (setf (,(cpu-register-accessor-name lo-key) ,*cpu-name*) (lo-byte ,combined-name)
	       (,(cpu-register-accessor-name hi-key) ,*cpu-name*) (hi-byte ,combined-name))))))

(defun compile-combined-register-sets (instr-spec)
  (remove 'nil (mapcar (fn (apply 'compile-combined-register-set instr-spec %))
		       '((:af :f :a)
			 (:bc :c :b)
			 (:de :e :d)
			 (:hl :l :h)))))

(defun compile-set-flags (instr-spec)
  (apply 'nconc
	 (mapcar (fn (compile-register-set % instr-spec))
		 '(:zero? :subtraction? :half-carry? :carry?))))

(defun compile-set-memory (instr-spec)
  (mapcar (fn `(mem-write! ,(first %) ,(second %) ,*memory-name*))
	  (aval :memory instr-spec)))

(defun compile-instr-spec-for-execute (instr-spec)
  (unless (akey? :cycles instr-spec)
    (warn "No CYCLES specified for ~A" instr-spec))
  `(let* ,(aval :bindings instr-spec)
     ,@(compile-combined-register-sets instr-spec)
     ,@(compile-set-memory instr-spec)
     (setf
      ,@(compile-set-flags instr-spec)
      ,@(compile-register-sets instr-spec)
      ,@(when-aval (ime? :ime? instr-spec)
	  `((cpu-ime? ,*cpu-name*) ,ime?)))
     ,(aval :cycles instr-spec)))

(defparameter *bit-indices* '(0 1 2 3 4 5 6 7))
(defparameter *register-codes* '(0 1 2 3 4 5 7))

(defun for-each-cartesian (fn &rest sets)
  (nlet rec ((sets sets)
	     (arguments ()))
    (if sets
	;; TODO: optimize
	(loop for element in (first sets) do
	  (rec (rest sets) (cons element arguments)))
	(funcall fn (reverse arguments)))))


(defun map-cartesian (fn &rest sets)
  (let ((result ()))
    (apply 'for-each-cartesian (fn (push (funcall fn %) result)) sets)
    result))

;; TODO: optimize by adding type declarations to cpu registers

(defun opcode-from-template (opcode-template opcode-parameter-bindings opcode-arguments)
  (apply 'logior opcode-template
	 (mapcar (fn (ash % (car %%)))
		 opcode-arguments
		 opcode-parameter-bindings)))

(defun opcode-bindings (opcode-parameter-bindings opcode-arguments)
  (mapcar (fn (list (first %) %%))
	  opcode-parameter-bindings
	  opcode-arguments))


(defun dd-register-pair-key (register-pair)
  (ecase register-pair
    (0 :bc)
    (1 :de)
    (2 :hl)
    (3 :sp)))
(defun qq-register-pair-key (register-pair)
  (ecase register-pair
    (0 :bc)
    (1 :de)
    (2 :hl)
    (3 :af)))

(defparameter *register-pair-codes* '(0 1 2 3))

(defun register8-keys-from-register-pair-key (combined-key)
  (ecase combined-key
    (:af '(:a :f))
    (:bc '(:b :c))
    (:de '(:d :e))
    (:hl '(:h :l))))

(defun register-key (code)
  (ecase code
    (#b111 :a)
    (#b0 :b)
    (#b1 :c)
    (#b010 :d)
    (#b011 :e)
    (#b100 :h)
    (#b101 :l)
    (#b110 :@hl)))

(defun map-opcodes (fn opcode-template opcode-list-parameters)
  (let ((opcode-parameter-lists (mapcar 'cdr opcode-list-parameters)))
    (apply 'map-cartesian
	   (lambda (arguments)
	     (let* ((opcode (opcode-from-template opcode-template opcode-list-parameters arguments)))
	       (apply fn opcode arguments)))
	   opcode-parameter-lists)))

(defun merge-instr-specs (old &rest new-instr-specs)
  (if new-instr-specs
      (let* ((new (first new-instr-specs)))
	(let* ((old-bindings (aval :bindings old))
	       (new-bindings (aval :bindings new))
	       (bindings (append old-bindings new-bindings))

	       (old-disassembly (aval :disassembly old))
	       (new-disassembly (aval :disassembly new))
	       (disassembly (amerge old-disassembly new-disassembly)))
	  (apply 'merge-instr-specs
		 (amerge (amerge old new)
			 (alist :bindings bindings
				:disassembly disassembly))
		 (rest new-instr-specs))))
      old))


(defvar *instr-specs* ())

(defun compile-instr-spec-effects (instr-spec)
  (compile-instr-effects
   (aval :bindings instr-spec)
   (akeep instr-spec
	  :a :b :c :d :e :f :h :l
	  :af :bc :de :hl
	  :sp :pc
	  :memory
	  :cycles
	  :zero? :carry? :half-carry? :subtraction?
	  :ime?)))
(defun compile-instr-spec-disassemble-instr (instr-spec)
  (compile-disassemble-instr 
   (aval :bindings instr-spec)
   (aval :disassembly instr-spec)))

(defun class-instr-specs (opcode-template opcode-list-parameters instr-spec-fn)
  "(Instr-spec-fn opcode opcode-arguments...) => instr-spec"
  (map-opcodes instr-spec-fn opcode-template opcode-list-parameters))

(defun ld-name (contents into)
  (list :ld into contents))

(defun key-description (key)
  (cond
    ((member key *register-keys*) key)
    ((eql key :imm8) "the 8-bit immediate operand")
    ((eql key :imm16) "the 16-bit immediate operand")
    ((eql key :@imm8) "the memory @(imm8 + #xFF00)")
    ((eql key :@imm16) "the memory @imm16")
    ((eql key :@bc) "the memory @BC")
    ((eql key :@de) "the memory @DE")
    ((eql key :@af) "the memory @AF")
    ((eql key :@hl) "the memory @HL")
    ((eql key :@c) "the memory @(C + #xFF00)")
    ((eql key :@hli) "the memory @HL (simultaneously post-incrementing HL)")
    ((eql key :@hld) "the memory @HL (simultaneously post-decrementing HL)")
    (t (error "unimplemented ~A" key))))
(defun ld-description (contents into)
  (format nil "Load the contents of ~A into ~A." (key-description contents) (key-description into)))

(defun compile-key-accessor (key)
  (cond
    ((member key *register-keys*) `(,(cpu-register-accessor-name key) ,*cpu-name*))
    ((eql key :imm8) *imm8-name*)
    ((eql key :@imm8) `(mem-byte (+ #xff00 ,*imm8-name*) ,*memory-name*))
    ((eql key :@imm16) `(mem-byte ,*imm16-name* ,*memory-name*))
    ((eql key :@bc) `(mem-byte (cpu-bc ,*cpu-name*) ,*memory-name*))
    ((eql key :@de) `(mem-byte (cpu-de ,*cpu-name*) ,*memory-name*))
    ((eql key :@af) `(mem-byte (cpu-af ,*cpu-name*) ,*memory-name*))
    ((member key '(:@hl :@hli :@hld)) `(mem-byte (cpu-hl ,*cpu-name*) ,*memory-name*))
    ((eql key :@c) `(mem-byte (+ #xff00 (cpu-c ,*cpu-name*)) ,*memory-name*))
    ((eql key :imm16) *imm16-name*)
    (t (error "unimplemented ~A" key))))
(defun key-set-instr-spec (key value-form)
  (let ((@hl-form `(((cpu-hl ,*cpu-name*) ,value-form))))
    (cond
      ((member key *register-keys*) (alist key value-form))
      ((eql key :@imm8) (alist :memory `(((+ #xff00 ,*imm8-name*) ,value-form))))
      ((eql key :@imm16) (alist :memory `((,*imm16-name* (lo-byte ,value-form))
					  ((1+ ,*imm16-name*) (hi-byte ,value-form)))))
      ((eql key :@bc) (alist :memory `(((cpu-bc ,*cpu-name*) ,value-form))))
      ((eql key :@de) (alist :memory `(((cpu-de ,*cpu-name*) ,value-form))))
      ((eql key :@af) (alist :memory `(((cpu-af ,*cpu-name*) ,value-form))))
      ((eql key :@hl) (alist :memory @hl-form))
      ((eql key :@c) (alist :memory `(((+ #xff00 (cpu-c ,*cpu-name*)) ,value-form))))
      ((eql key :@hli) (alist :memory @hl-form
			      :hl `(1+ (cpu-hl ,*cpu-name*))))
      ((eql key :@hld) (alist :memory @hl-form
			      :hl `(1- (cpu-hl ,*cpu-name*))))
      (t (error "unimplemented ~A" key)))))

(defun imm8-instr-spec ()
  (alist
   :bindings `((,*imm8-name* (mem-byte (1+ ,*pc-name*) ,*memory-name*)))
   :disassembly (alist :imm8 `(register8-text ,*imm8-name*))))
(defun s8-instr-spec ()
  (merge-instr-specs
   (imm8-instr-spec)
   (alist :bindings `((,*s8-name* (s8 ,*imm8-name*)))
	  :disassembly (alist :s8 *s8-name*))))
(defun imm16-instr-spec ()
  (alist
   :bindings `((,*imm16-name* ,(compile-16bit-memory-access `(1+ ,*pc-name*))))
   :disassembly (alist :imm16 `(register16-text ,*imm16-name*))))

(defun instr-spec-defaults (&key name description instr-size long?
			      opcode cycles disassembly)
  (amerge
   (alist :long? long?)
   (when instr-size
     (alist :pc `(+ ,*pc-name* ,instr-size)
	    :instr-size instr-size))
   (when name
     (alist :name name))
   (when description
     (alist :description description))
   (when disassembly
     (alist :disassembly disassembly))
   (when opcode
     (alist :opcode opcode))
   (when cycles
     (alist :cycles cycles))))

(defparameter *1+-cycle-keys* '(:imm8 :@c :@af :@bc :@de :@hl :@hli :@hld))
(defun ld-8bit-cycles (contents-key into-key)
  (+ 1
     (if (member contents-key *1+-cycle-keys*) 1 0)
     (if (member contents-key *register16-keys*) 1 0)
     (if (member into-key *1+-cycle-keys*) 1 0)
     (if (member contents-key '(:imm16 :@imm8)) 2 0)
     (if (member into-key '(:imm16 :@imm8)) 2 0)
     (if (eql contents-key :@imm16) 3 0)
     (if (eql into-key :@imm16) 3 0)))

(defun ld-instr-spec (opcode contents-key into-key)
  (let* ((imm8? (or (member into-key '(:imm8 :@imm8))
		    (member contents-key '(:imm8 :@imm8))))
	 (imm16? (or (member into-key '(:imm16 :@imm16))
		     (member contents-key '(:imm16 :@imm16))))
	 (cycles (ld-8bit-cycles contents-key into-key))
	 (instr-size (cond
		       (imm16? 3)
		       (imm8? 2)
		       (t 1))))
    (merge-instr-specs
     (instr-spec-defaults :opcode opcode
			  :cycles cycles
			  :instr-size instr-size)
     (when imm8? (imm8-instr-spec))
     (when imm16? (imm16-instr-spec))
     (when (member contents-key '(:@hli))
       (alist :hl `(1+ (cpu-hl ,*cpu-name*))))
     (when (member contents-key '(:@hld))
       (alist :hl `(1- (cpu-hl ,*cpu-name*))))
     (alist
      :name (ld-name contents-key into-key)
      :description (ld-description contents-key into-key))
     (key-set-instr-spec into-key (compile-key-accessor contents-key)))))


(defun ld-8bit-class-instr-specs (opcode opcode-list-parameters fn)
  (class-instr-specs
   opcode opcode-list-parameters
   (lambda (opcode &rest register-codes)
     (apply fn opcode (mapcar 'register-key register-codes)))))

(defun @imm8-disassembly-instr-spec ()
  (alist :disassembly (alist :addr `(+ #xff00 ,*imm8-name*))))

;; S 2.1
(defun ld-8bit-instr-specs ()
  (append
   ;; LD r, r'
   (ld-8bit-class-instr-specs
    #b01000000 (alist 3 *register-codes*
		      0 *register-codes*)
    (lambda (opcode from-register into-register)
      (ld-instr-spec opcode from-register into-register)))

   ;; LD r, imm8
   (ld-8bit-class-instr-specs
    #b00000110 (alist 3 *register-codes*)
    (lambda (opcode register)
      (ld-instr-spec opcode :imm8 register)))

   ;; LD r, (HL)
   (ld-8bit-class-instr-specs
    #b01000110 (alist 3 *register-codes*)
    (lambda (opcode register)
      (ld-instr-spec opcode :@hl register)))

   ;; LD (HL), r
   (ld-8bit-class-instr-specs
    #b01110000 (alist 0 *register-codes*)
    (lambda (opcode register)
      (ld-instr-spec opcode register :@hl)))

   (list
    (ld-instr-spec #b00110110 :imm8 :@hl)
    (ld-instr-spec #b00001010 :@bc :a)
    (ld-instr-spec #b00011010 :@de :a)
    (ld-instr-spec #b11110010 :@c :a)
    (ld-instr-spec #b11100010 :a :@c)
    (merge-instr-specs
     (ld-instr-spec #b11110000 :@imm8 :a)
     (@imm8-disassembly-instr-spec))

    (merge-instr-specs
     (ld-instr-spec #b11100000 :a :@imm8)
     (@imm8-disassembly-instr-spec))
    
    (merge-instr-specs
     (ld-instr-spec #b11111010 :@imm16 :a)
     (alist :disassembly (alist :addr 'imm16)))
    (merge-instr-specs
     (ld-instr-spec #b11101010 :a :@imm16)
     (alist :disassembly (alist :addr 'imm16)))

    ;; TODO: optimize create HL binding
    (ld-instr-spec #b00101010 :@hli :a)
    (ld-instr-spec #b00111010 :@hld :a)

    (ld-instr-spec #b00000010 :a :@bc)
    (ld-instr-spec #b00010010 :a :@de)

    (ld-instr-spec #b00100010 :a :@hli)
    (ld-instr-spec #b00110010 :a :@hld))))

(defun imm16 (addr memory)
  (combined-register (mem-byte (1+ addr) memory) (mem-byte addr memory)))

;; S2.2
(defun ld-16bit-instr-specs ()
  (append
   ;; LD dd, nn
   (map-opcodes
    (lambda (opcode register-pair)
      (ld-instr-spec opcode :imm16 (dd-register-pair-key register-pair)))
    #b00000001 (alist 4 *register-pair-codes*))
   
   (list
    ;; LD SP, HL
    (ld-instr-spec #b11111001 :hl :sp))

   ;; PUSH qq
   (map-opcodes
    (lambda (opcode register-pair)
      (let* ((combined-key (qq-register-pair-key register-pair)))
	(merge-instr-specs
	 (instr-spec-defaults)
	 (push-instr-spec combined-key)
	 (pc-next-instr)
	 (alist :opcode opcode
		:name (list :push combined-key)
		:description (format nil "Push the contents of register pair ~A onto the stack." combined-key)
		:cycles 4))))
    #b11000101 (alist 4 *register-pair-codes*))

   ;; POP qq
   (map-opcodes
    (lambda (opcode register-pair)
      (let* ((register-pair-key (qq-register-pair-key register-pair)))
	(merge-instr-specs
	 (instr-spec-defaults)
	 (pop-instr-spec register-pair-key)
	 (pc-next-instr)
	 (alist :opcode opcode
		:name (list :pop register-pair-key)
		:description (format nil "Pop the 16-bit value off the stack into register pair ~A." register-pair-key)
		:cycles 3))))
    #b11000001 (alist 4 *register-pair-codes*))

   (list
    ;; LDHL SP, s8
    (merge-instr-specs
     (instr-spec-defaults
      :opcode #b11111000
      :name (list :ld :hl :sp+s8)
      :description "Add signed imm8 value to sp and store the
result in HL."
      :cycles 3)
     (s8-instr-spec)
     (pc-next-instr :instr-size 2)
     (alist :bindings '((sp (cpu-sp cpu)))
	    :hl '(+ s8 sp)

	    ;; 16-bit addition flags?
	    :zero? nil
	    :subtraction? nil
	    :half-carry? '(bit-carry? 11 sp s8)
	    :carry? '(bit-carry? 15 sp s8)))
    
    (merge-instr-specs
     (ld-instr-spec #b00001000 :sp :@imm16)
     (alist :disassembly (alist :addr 'imm16))))))

(defun push-value-instr-spec (value-form &key (sp-name 'sp) (sp-2-name 'sp-2) (value-name 'value))
  (alist :bindings `((,sp-name (cpu-sp ,*cpu-name*))
		     (,sp-2-name (- ,sp-name 2))
		     (,value-name ,value-form))
	 :sp sp-2-name
	 :memory `(((1- ,sp-name) (hi-byte ,value-name))
		   (,sp-2-name (lo-byte ,value-name)))))
(defun push-instr-spec (register-pair-key &key (sp-name 'sp) (sp-2-name 'sp-2) (value-name 'value))
  (push-value-instr-spec `(,(cpu-register-accessor-name register-pair-key) ,*cpu-name*)
			 :sp-name sp-name
			 :sp-2-name sp-2-name
			 :value-name value-name))

(defun pop-instr-spec (register-pair-key &key (sp-name 'sp) (sp+-name 'sp+))
  (alist :bindings `((,sp-name (cpu-sp ,*cpu-name*))
		     (,sp+-name (1+ ,sp-name)))
	 :sp `(+ ,sp-name 2)
	 register-pair-key `(combined-register (mem-byte ,sp+-name ,*memory-name*)
					       (mem-byte ,sp-name ,*memory-name*))))

(defun bit-carry? (from-bit-index a b)
  (let* ((bit-index (1+ from-bit-index))
	 (mask (1- (expt 2 bit-index))))
    (not (zerop (logand (+ (logand mask a)
			   (logand mask b))
			(expt 2 bit-index))))))

(defun alu-name (alu-op-key contents-key)
  (list alu-op-key contents-key))
(defun alu-description (alu-op-key contents-key)
  (let* ((alu-op-name (ecase alu-op-key
			(:add "add")
			(:adc "add with carry")
			(:sub "subtraction")
			(:sbc "subtraction with carry/borrow")
			(:and "logical and")
			(:or "logical or")
			(:xor "logical exclusive-or")
			(:cp "comparison (-)"))))
    (format nil "Performs ~A with A as the first argument and ~A as the second. ~A" alu-op-name (key-description contents-key)
	    (if (eql :cp alu-op-key)
		"Dicards the result."
		"Stores the result in A."))))

(defun compile-alu-op (alu-op-key &key (left *a-name*) (right *data-name*))
  (ecase alu-op-key
    (:add `(logand #xff (+ ,left ,right)))
    (:adc `(logand #xff (+ ,left ,right ,*carry-bit-name*)))
    ((:cp :sub) `(logand #xff (- ,left ,right)))
    (:sbc `(logand #xff (- ,left ,right ,*carry-bit-name*)))
    (:and `(logand ,left ,right))
    (:or `(logior ,left ,right))
    (:xor `(logxor ,left ,right))))

(defun alu-op-flags (alu-op-key &key (left *a-name*) (right *data-name*))
  (ecase alu-op-key
    (:add (alist :carry? `(bit-carry? 7 ,left ,right)
		 :half-carry? `(bit-carry? 3 ,left ,right)
		 :subtraction? nil
		 :zero? `(zerop ,*result-name*)))
    (:adc (alist :carry? `(bit-carry-cy? 7 ,left ,right ,*carry-bit-name*)
		 :half-carry? `(bit-carry-cy? 3 ,left ,right ,*carry-bit-name*)
		 :subtraction? nil
		 :zero? `(zerop ,*result-name*)))

    ((:cp :sub) (alist :carry? `(bit-borrow? 8 ,left ,right)
		       :half-carry? `(bit-borrow? 4 ,left ,right)
		       :subtraction? t
		       :zero? `(zerop ,*result-name*)))
    (:sbc (alist :carry? `(bit-borrow-cy? 8 ,left ,right ,*carry-bit-name*)
		 :half-carry? `(bit-borrow-cy? 4 ,left ,right ,*carry-bit-name*)
		 :subtraction? t
		 :zero? `(zerop ,*result-name*)))

    (:and (alist :zero? `(zerop ,*result-name*)
		 :half-carry? t
		 :carry? nil
		 :subtraction? nil))
    ((:or :xor) (alist :zero? `(zerop ,*result-name*)
		       :half-carry? nil
		       :carry? nil
		       :subtraction? nil))))

(defun 8bit-alu-instr-spec (opcode alu-op-key contents-key)
  (let* ((imm8? (member contents-key '(:imm8 :@imm8)))
	 (imm16? (member contents-key '(:imm16 :@imm16)))
	 (memory-ref? (member contents-key '(:@imm8 :@imm16 :@hl)))
	 (carry? (member alu-op-key '(:adc :sbc)))
	 (cycles (+ 1
		    (if imm8? 1 0)
		    (if imm16? 2 0)
		    (if memory-ref? 1 0)))
	 (instr-size (cond
		       (imm16? 3)
		       (imm8? 2)
		       (t 1))))
    (merge-instr-specs
     (instr-spec-defaults :opcode opcode
			  :cycles cycles
			  :name (alu-name alu-op-key contents-key)
			  :description (alu-description alu-op-key contents-key)
			  :instr-size instr-size)
     (when imm8? (imm8-instr-spec))
     (when imm16? (imm16-instr-spec))
     (a-binding)
     (data-bindings contents-key)
     (alist :bindings (when carry? `((,*carry-bit-name* (cpu-carry-bit ,*cpu-name*)))))
     (result-bindings (compile-alu-op alu-op-key))
     (alu-op-flags alu-op-key)
     (unless (eql :cp alu-op-key)
       (key-set-instr-spec :a 'result)))))

(defun 8bit-inc-op-instr-spec (opcode inc-key contents-key)
  (let* ((memory-ref? (member contents-key '(:@hl)))
	 (cycles (+ 1
		    (if memory-ref? 2 0)))
	 (alu-op-key (ecase inc-key
		       (:inc :add)
		       (:dec :sub))))
    (merge-instr-specs
     (instr-spec-defaults 
      :name (list inc-key contents-key)
      :description(format nil "~A the contents of ~A."
			  (ecase inc-key
			    (:inc "Increments")
			    (:dec "Decrements"))
			  (key-description contents-key))
      :opcode opcode
      :cycles cycles)
     (data-bindings contents-key)
     (result-bindings (compile-alu-op alu-op-key :left *data-name* :right 1))
     (pc-next-instr)
     (aremove (alu-op-flags alu-op-key :left *data-name* :right 1) :carry?)
     (key-set-instr-spec contents-key 'result))))

(defun 8bit-alu-op-instr-specs (alu-op-key r-opcode-template imm8-opcode @hl-opcode)
  (append
   (map-opcodes
    (lambda (opcode register)
      (8bit-alu-instr-spec opcode alu-op-key (register-key register)))
    r-opcode-template (alist 0 *register-codes*))
   (list
    (8bit-alu-instr-spec imm8-opcode alu-op-key :imm8)
    (8bit-alu-instr-spec @hl-opcode alu-op-key :@hl))))

(defun 8bit-inc-op-instr-specs (inc-op-key r-opcode-template @hl-opcode)
  (append
   (map-opcodes
    (lambda (opcode register)
      (8bit-inc-op-instr-spec opcode inc-op-key (register-key register)))
    r-opcode-template (alist 3 *register-codes*))
   (list
    (8bit-inc-op-instr-spec @hl-opcode inc-op-key :@hl))))

;; S2.3
(defun 8bit-alu-instr-specs ()
  (append
   (8bit-alu-op-instr-specs :add #b10000000 #b11000110 #b10000110)
   (8bit-alu-op-instr-specs :adc #b10001000 #b11001110 #b10001110)
   (8bit-alu-op-instr-specs :sub #b10010000 #b11010110 #b10010110)
   (8bit-alu-op-instr-specs :sbc #b10011000 #b11011110 #b10011110)
   (8bit-alu-op-instr-specs :and #b10100000 #b11100110 #b10100110)
   (8bit-alu-op-instr-specs :or  #b10110000 #b11110110 #b10110110)
   (8bit-alu-op-instr-specs :xor #b10101000 #b11101110 #b10101110)
   (8bit-alu-op-instr-specs :cp  #b10111000 #b11111110 #b10111110)

   (8bit-inc-op-instr-specs :inc #b00000100 #b00110100)
   (8bit-inc-op-instr-specs :dec #b00000101 #b00110101)))

(defun bit-carry-cy? (bit-index a b cy)
  ;; TODO: Check up on how to check for bit-carry?
  (or (bit-carry? bit-index a b)
      (bit-carry? bit-index (+ a b) cy)))

(defun bit-borrow? (bit-index a b)
  (let* ((mask (1- (expt 2 bit-index))))
    (< (logand a mask)
       (logand b mask))))

(defun bit-borrow-cy? (bit-index a b cy)
  ;; TODO: Check up on how to check for bit-borrow?
  (or (bit-borrow? bit-index a b)
      (bit-borrow? bit-index (- a b) cy)))

(defun 16bit-add-instr-spec (opcode contents-key)
  (merge-instr-specs
   (instr-spec-defaults 
    :opcode opcode
    :cycles 2)
   (pc-next-instr)
   (alist
    :bindings `((hl (cpu-hl ,*cpu-name*))
		(data ,(compile-key-accessor contents-key))
		(result (logand #xffff (+ hl data))))
    :name (list :add :hl contents-key)
    :description (format nil "Add HL to ~A. Store the result in HL."
			 (key-description contents-key))
    :hl 'result
    :carry? `(bit-carry? 15 hl data)
    :half-carry? `(bit-carry? 11 hl data)
    :subtraction? nil)))

(defun 16-bit-inc-op-instr-spec (opcode inc-op-key contents-key)
  (merge-instr-specs
   (instr-spec-defaults :opcode opcode
			:cycles 2)
   (pc-next-instr)
   (alist :bindings `((data ,(compile-key-accessor contents-key))
		      (result (logand #xffff (,(ecase inc-op-key
						 (:inc '1+)
						 (:dec '1-))
					       data))))
	  :name (list :inc contents-key)
	  :decsription (format nil "~A the contents of ~A."
			       (ecase inc-op-key
				 (:inc "Increment")
				 (:dec "Decrement"))
			       (key-description contents-key))
	  contents-key 'result)))

;; S2.4
(defun 16bit-alu-op-instr-specs ()
  (append
   ;; ADD HL, ss
   (map-opcodes
    (lambda (opcode register-pair)
      (16bit-add-instr-spec opcode (dd-register-pair-key register-pair)))
    #b00001001 (alist 4 *register-pair-codes*))
   (list
    ;; ADD SP, imm8
    (merge-instr-specs
     (instr-spec-defaults :opcode #b11101000
			  :cycles 4)
     (imm8-instr-spec)
     (pc-next-instr :instr-size 2)
     (alist
      :bindings `((sp (cpu-sp cpu))
		  (result (logand #xffff (+ sp imm8))))
      :name (list :add :sp :imm8)
      :description "Add SP to the imm8 operand. Store the result in SP."
      :sp 'result
      :zero? nil
      :carry? `(bit-carry? 15 sp imm8)
      :half-carry? `(bit-carry? 11 sp imm8)
      :subtraction? nil)))

   ;; INC ss
   (map-opcodes
    (lambda (opcode register-pair)
      (16-bit-inc-op-instr-spec opcode :inc (dd-register-pair-key register-pair)))
    #b00000011 (alist 4 *register-pair-codes*))

   ;; DEC ss
   (map-opcodes
    (lambda (opcode register-pair)
      (16-bit-inc-op-instr-spec opcode :dec (dd-register-pair-key register-pair)))
    #b00001011 (alist 4 *register-pair-codes*))))

(defun bit7? (byte)
  (not (zerop (logand #x80 byte))))
(defun cpu-carry-bit (cpu)
  (if (cpu-carry? cpu) 1 0))

(defun a-binding ()
  (alist :bindings `((,*a-name* (cpu-a ,*cpu-name*)))))
(defun pc-next-instr (&key (instr-size 1))
  (alist :pc `(+ ,instr-size ,*pc-name*)
	 :instr-size instr-size))
(defun pc-jump (addr-form &key condition-form instr-size)
  (alist :pc (if condition-form
		 `(if ,condition-form ,addr-form (+ ,instr-size ,*pc-name*))
		 addr-form)
	 :disassembly (alist :addr addr-form)
	 :instr-size instr-size
	 :jump? t))

(defun rotate-flags (bit-set?-form &optional zerop-result-name)
  (alist :carry? bit-set?-form
	 :half-carry? nil
	 :zero? (when zerop-result-name
		  `(zerop ,zerop-result-name))
	 :subtraction? nil))
(defun bit-set? (bit-index byte)
  (logbitp bit-index byte))

(defparameter *bit-name* 'bit)
(defparameter *bit?-name* 'bit?)
(defun rotate-bindings (bit-index byte-form &key bit-name)
  (alist :bindings `((,*bit?-name* (bit-set? ,bit-index ,byte-form))
		     ,@(when bit-name
			 `((,bit-name (if ,*bit?-name* 1 0)))))))

(defun rc-description (key direction)
  (let* ((description (key-description key)))
    (format nil "Rotate the contents of ~A to the ~A 1 bit,
placing the ~Ath bit of ~A into the ~Ath bit of ~A and the into carry bit."
	    description
	    (ecase direction
	      (:left "left")
	      (:right "right"))
	    (ecase direction
	      (:left 7)
	      (:right 0))
	    description
	    (ecase direction
	      (:left 0)
	      (:right 7))
	    description)))
(defun rotate-description (key direction)
  (let* ((description (key-description key)))
    (format nil "Rotate the contents of ~A to the ~A 1 bit,
placing the carry bit in the ~Ath bit of ~A, and setting the carry bit to the ~Ath bit of ~A."
	    description
	    (ecase direction
	      (:left "left")
	      (:right "right"))
	    (ecase direction
	      (:left 0)
	      (:right 7))
	    description
	    (ecase direction
	      (:left 7)
	      (:right 0))
	    description)))

(defun data-bindings (key)
  (alist :bindings `((,*data-name* ,(compile-key-accessor key)))))

(defun result-bindings (form)
  (alist :bindings `((,*result-name* ,form))))

(defun rlc-bindings (key)
  (merge-instr-specs
   (data-bindings key)
   (rotate-bindings 7 *data-name* :bit-name *bit-name*)
   (result-bindings `(logior (logand #xff (ash ,*data-name* 1)) ,*bit-name*))))
(defun rlc-instr-spec (key &optional zero?)
  (merge-instr-specs
   (alist :description (rc-description key :left))
   (rlc-bindings key)
   (key-set-instr-spec key 'result)
   (rotate-flags 'bit? (when zero? 'result))))

(defun shift-left-bindings (key)
  (merge-instr-specs
   (data-bindings key)
   (rotate-bindings 7 *data-name*)))

(defun rl-bindings (key)
  (merge-instr-specs
   (shift-left-bindings key)
   (result-bindings `(logior (logand #xff (ash ,*data-name* 1)) (cpu-carry-bit ,*cpu-name*)))))
(defun rl-instr-spec (key &optional zero?)
  (merge-instr-specs
   (alist :description (rotate-description key :left))
   (rl-bindings key)
   (key-set-instr-spec key 'result)
   (rotate-flags 'bit? (when zero? 'result))))


(defun rrc-bindings (key)
  (merge-instr-specs
   (data-bindings key)
   (rotate-bindings 0 *data-name* :bit-name *bit-name*)
   (result-bindings `(logior (ash ,*data-name* -1) (ash ,*bit-name* 7)))))
(defun rrc-instr-spec (key &optional zero?)
  (merge-instr-specs
   (alist :description (rc-description key :right))
   (rrc-bindings key)
   (key-set-instr-spec key 'result)
   (rotate-flags 'bit? (when zero? 'result))))

(defun shift-right-bindings (key)
  (merge-instr-specs
   (data-bindings key)
   (rotate-bindings 0 *data-name*)))

(defun rr-bindings (key)
  (merge-instr-specs
   (shift-right-bindings key)
   (result-bindings `(logior (ash ,*data-name* -1) (ash (cpu-carry-bit ,*cpu-name*) 7)))))
(defun rr-instr-spec (key &optional zero?)
  (merge-instr-specs
   (alist :description (rotate-description key :right))
   (rr-bindings key)
   (key-set-instr-spec key 'result)
   (rotate-flags 'bit? (when zero? 'result))))

(defparameter *register-and-@hl-codes* (list* 6 *register-codes*))
(defun rotate/shift-parameter-bindings ()
  (alist 0 *register-and-@hl-codes*))

(defun concat (&rest things)
  (with-output-to-string (s)
    (loop for e in things do (format s "~A" e))))


(defun long-instr-size (&optional (num-immediate-bytes 0))
  (merge-instr-specs
   (pc-next-instr :instr-size (+ num-immediate-bytes 2))
   (alist :long? t)))
(defun long-rotate/shift-instr-spec (name key)
  (merge-instr-specs
   (alist :name (list name key)
	  :cycles (if (eql key :@hl)
		      4
		      2))
   (long-instr-size)))

(defun map-rotate/shift-opcodes (fn opcode-template)
  (map-opcodes (lambda (opcode register)
		 (funcall fn opcode (register-key register)))
	       opcode-template (rotate/shift-parameter-bindings)))

(defun rotate-group-instr-specs (name r-opcode-template instr-spec-fn)
  (let* ((opcode-parameter-bindings (rotate/shift-parameter-bindings))
	 (ra-opcode (opcode-from-template r-opcode-template opcode-parameter-bindings '(7)))
	 (ra-name (intern (concat name "A") :keyword)))
    (append
     (list
      ;; R a
      (merge-instr-specs
       (instr-spec-defaults :opcode ra-opcode
			    :cycles 1
			    :name ra-name
			    :instr-size 1)
       (funcall instr-spec-fn :a)))

     ;; R r, R (HL)
     (map-rotate/shift-opcodes
      (lambda (opcode key)
	(merge-instr-specs
	 (instr-spec-defaults :opcode opcode)
	 (long-rotate/shift-instr-spec name key)
	 (funcall instr-spec-fn key t)))
      r-opcode-template))))

(defun sla-description (key)
  (let* ((description (key-description key)))
    (format nil "Shift the contents of ~A 1 bit to the left.
Places the 7th bit of ~A into the carry-bit and places 0 in the first bit of ~A."
	    description description description)))
(defun sra-description (key)
  (let* ((description (key-description key)))
    (format nil "Shift the contents of ~A 1 bit to the right.
Places the 0th bit of ~A into the carry-bit and leaves the 7th bit of ~A unchanged."
	    description description description)))
(defun srl-description (key)
  (let* ((description (key-description key)))
    (format nil "Shift the contents of ~A 1 bit to the right.
Places the 0th bit of ~A into the carry-bit and sets the 7th bit to 0."
	    description description)))

(defun shift/swap-instr-specs (opcode-template name description-fn data-bindings-fn result-form flags)
  (map-rotate/shift-opcodes
   (lambda (opcode key)
     (merge-instr-specs
      (instr-spec-defaults
       :opcode opcode
       :description (funcall description-fn key))
      (long-rotate/shift-instr-spec name key)
      (funcall data-bindings-fn key)
      (result-bindings result-form)
      (key-set-instr-spec key 'result)
      flags))
   opcode-template))

(defun shift-instr-specs (opcode-template name description-fn shift-bindings-fn result-form)
  (shift/swap-instr-specs opcode-template name description-fn shift-bindings-fn result-form (rotate-flags 'bit? 'result)))

;; S 2.5
(defun rotate-shift-instr-specs ()
  (append
   ;; RLCA, RLC r, RLC (HL)
   (rotate-group-instr-specs :rlc #b00000000 'rlc-instr-spec)
   ;; RLA, RL r, RL (HL)
   (rotate-group-instr-specs :rl #b00010000 'rl-instr-spec)
   ;; RRCA, RRC r, RRC (HL)
   (rotate-group-instr-specs :rrc #b00001000 'rrc-instr-spec)
   ;; RRA, RR r, RR (HL)
   (rotate-group-instr-specs :rr #b00011000 'rr-instr-spec)

   ;; SLA r, SLA (HL)
   (shift-instr-specs #b00100000 :sla 'sla-description 'shift-left-bindings '(logand #xff (ash data 1)))
   ;; SRA r, SRA (HL)
   (shift-instr-specs #b00101000 :sra 'sra-description 'shift-right-bindings '(shift-right-arithmetic data))

   ;; SRL r, SRL (HL)
   (shift-instr-specs #b00111000 :srl 'srl-description 'shift-right-bindings '(shift-right-logical data))

   ;; SWAP r, SWAP (HL)
   (shift/swap-instr-specs #b00110000
			   :swap
			   (fn (format nil "Swaps the lower 4 bits with the upper 4 bits of ~A." (key-description %)))
			   'data-bindings
			   '(swap data)
			   (alist :carry? nil
				  :subtraction? nil
				  :half-carry? nil
				  :zero? '(zerop result)))))

(defun shift-right-arithmetic (byte)
  (bit-set 7 (ash byte -1) (bit-value byte 7)))
(defun shift-right-logical (byte)
  (logand #x7f (ash byte -1)))

(defun lo-nibble (byte)
  (logand #xf byte))
(defun hi-nibble (byte)
  (ash (logand #xf0 byte) -4))
(defun swap (byte)
  (let* ((lo (lo-nibble byte))
	 (hi (hi-nibble byte)))
    (logior (ash lo 4) hi)))

(defun bit-set (bit-index byte bit-value)
  (if (= 1 bit-value)
      (logior (ash 1 bit-index) byte)
      (logand (- #xff (ash 1 bit-index)) byte)))

;; S2.6
(defun map-bit-opcodes (fn opcode-template)
  (map-opcodes 
   (lambda (opcode bit-index register)
     (let* ((key (register-key register)))
       (funcall fn opcode bit-index key)))
   opcode-template (alist 0 *register-and-@hl-codes*
			  3 *bit-indices*)))

(defun bit-description (bit-index key)
  (format nil "Copies the complement of bit ~A of ~A into the zero flag."
	  bit-index (key-description key)))

(defun set-description (bit-index key)
  (format nil "Sets the bit ~A of ~A to be 1."
	  bit-index (key-description key)))
(defun res-description (bit-index key)
  (format nil "Sets the bit ~A of ~A to be 0."
	  bit-index (key-description key)))

(defun bit-instr-spec (name description-fn opcode bit-index key)
  (merge-instr-specs
   (instr-spec-defaults
    :opcode opcode
    :name (list name bit-index key)
    :description (funcall description-fn bit-index key))
   (long-instr-size)
   (data-bindings key)))

(defun bit-set-instr-spec (name description-fn bit-value opcode bit-index key)
  (merge-instr-specs
   (instr-spec-defaults
    :opcode opcode
    :name (list name bit-index key)
    :description (funcall description-fn bit-index key))
   (long-instr-size)
   (data-bindings key)
   (alist :cycles (if (eql key :@hl) 4 2))
   (key-set-instr-spec key `(bit-set ,bit-index data ,bit-value))))

(defun bit-instr-specs ()
  (append
   ;; BIT b r, BIT b (HL)
   (map-bit-opcodes
    (lambda (opcode bit-index key)
      (merge-instr-specs
       (bit-instr-spec :bit 'bit-description opcode bit-index key)
       (alist :cycles (if (eql key :@hl) 3 2))
       (alist :half-carry? t
	      :subtraction? nil
	      :zero? `(not (logbitp ,bit-index data)))))
    #b01000000)

   ;; SET b r, SET b (HL)
   (map-bit-opcodes
    (lambda (opcode bit-index key)
      (bit-set-instr-spec :set 'res-description 1 opcode bit-index key))
    #b11000000)

   ;; RES b r, RES b (HL)
   (map-bit-opcodes
    (lambda (opcode bit-index key)
      (bit-set-instr-spec :res 'res-description 0 opcode bit-index key))
    #b10000000)))

(defparameter *condition-codes* '(0 1 2 3))
(defun condition-key (condition-code)
  (ecase condition-code
    (0 :not-zero?)
    (1 :zero?)
    (2 :carry?)
    (3 :not-carry?)))
(defun compile-condition-key-test (condition-key)
  (ecase condition-key
    (:not-zero? `(not (cpu-zero? ,*cpu-name*)))
    (:zero? `(cpu-zero? ,*cpu-name*))
    (:carry? `(cpu-carry? ,*cpu-name*))
    (:not-carry? `(not (cpu-carry? ,*cpu-name*)))))


(defun condition-key-description (condition-key)
  (ecase condition-key
    (:zero? "the zero? flag is set")
    (:not-zero? "the zero? flag is not set")
    (:carry? "the carry? flag is set")
    (:not-carry? "the carry? flag is not set")))

(defun condition?-bindings (condition-key)
  (alist :bindings `((condition? ,(compile-condition-key-test condition-key)))))

(defun jump-conditionally-instr-spec (condition-key instr-size addr-form min-cycles)
  (merge-instr-specs
   (condition?-bindings condition-key)
   (pc-jump addr-form :condition-form 'condition? :instr-size instr-size)
   (alist :cycles `(+ ,min-cycles
		      (if condition? 1 0)))))

(defun map-jump-conditional-opcodes (fn opcode-template)
  (map-opcodes
   (lambda (opcode condition-code)
     (funcall fn opcode (condition-key condition-code)))
   opcode-template (alist 3 *condition-codes*)))

(defun compile-16bit-memory-access (addr-form)
  `(imm16 ,addr-form ,*memory-name*))
(defun compile-imm16-access (&optional long-instruction?)
  (compile-16bit-memory-access `(+ ,(if long-instruction? 2 1) ,*pc-name*)))

(defun relative-addr-instr-spec ()
  (merge-instr-specs
   (s8-instr-spec)
   (alist :bindings `((,*addr-name* (+ 2 ,*pc-name* ,*s8-name*)))
	  :disassembly (alist :addr *addr-name*))))

;; S2.7
(defun jump-instr-specs ()
  (append
   (list
    ;; JP nn
    (merge-instr-specs
     (instr-spec-defaults
      :name (list :jp :imm16)
      :description "Load the imm16 value into the PC."
      :opcode #b11000011
      :cycles 4)
     (imm16-instr-spec)
     (pc-jump 'imm16)))

   ;; JP cc, nn
   (map-jump-conditional-opcodes
    (lambda (opcode condition-key)
      (merge-instr-specs
       (instr-spec-defaults
	:name (list :jp condition-key :imm16)
	:description (format nil "Load the imm16 value into the PC if ~A."
			     (condition-key-description condition-key))
	:opcode opcode)
       (alist :disassembly (alist :imm16 (compile-imm16-access)))
       (jump-conditionally-instr-spec condition-key 3 (compile-imm16-access) 3)))
    #b11000010)

   (list
    ;; JR imm8
    (merge-instr-specs
     (instr-spec-defaults
      :name (list :jr :imm8)
      :description "Load the signed imm8 value+PC+2 into the PC."
      :opcode #b00011000
      :cycles 3)
     (relative-addr-instr-spec)
     (pc-jump *addr-name*)))

   ;; JR cc imm8
   (map-jump-conditional-opcodes
    (lambda (opcode condition-key)
      (merge-instr-specs
       (instr-spec-defaults
	:name (list :jr condition-key :imm8)
	:description (format nil "Load the signed imm8 value+PC+2 into PC if ~A."
			     (condition-key-description condition-key))
	:opcode opcode)
       ;; TODO: optimize by only accessing memory if condition? is true
       (relative-addr-instr-spec)
       (jump-conditionally-instr-spec condition-key 2 *addr-name* 2)))
    #b00100000)

   (list
    ;; JP (HL)
    (merge-instr-specs
     (instr-spec-defaults
      :name (list :jp :@hl)
      :description "Copy HL into PC."
      :opcode #b11101001
      :instr-size 1
      :cycles 1)
     (pc-jump '(cpu-hl cpu))))))

;; S2.8
(defun call-and-return-instr-specs ()
  (append
   (list
    ;; CALL imm16
    (merge-instr-specs
     (instr-spec-defaults
      :name (list :call :imm16)
      :description "PUSH address of next instruction onto the stack. Jump to imm16."
      :opcode #b11001101
      :cycles 6)
     (imm16-instr-spec)
     (push-value-instr-spec `(+ ,*pc-name* 3))
     (pc-jump 'imm16 :instr-size 3)))

   ;; CALL cc imm16
   (map-jump-conditional-opcodes
    (lambda (opcode condition-key)
      (merge-instr-specs
       (instr-spec-defaults
	:name (list :call condition-key :imm16)
	:description (format nil "If ~A, PUSH address of next instruction onto the stack and Jump to imm16.
Otherwise, increment PC to the next instruction."
			     (condition-key-description condition-key))
	:opcode opcode)
       ;; TODO: optimize by only accessing imm16 if condition? is true.
       (imm16-instr-spec)
       (condition?-bindings condition-key)
       (alist
	:bindings `((sp (cpu-sp cpu))
		    (sp-1 (1- sp))
		    (sp-2 (- sp 2))
		    (pc+3 (+ pc 3)))
	:sp '(if condition? sp-2 sp)
	;; TODO: optimize by not setting memory if condition? is false
	:memory `((sp-1 (if condition? (hi-byte pc+3) (mem-byte sp-1 memory)))
		  (sp-2 (if condition? (lo-byte pc+3) (mem-byte sp-2 memory))))
	:cycles '(if condition? 6 3))
       (pc-jump 'imm16 :condition-form 'condition? :instr-size 3)))
    #b11000100)

   (list
    ;; RET
    (merge-instr-specs
     (instr-spec-defaults
      :name :ret
      :description "POP the value off the stack into the PC."
      :opcode #b11001001
      :cycles 4)
     (alist :jump? t)
     (pop-instr-spec :pc))

    ;; RETI
    (merge-instr-specs
     (alist :name :reti
	    :description "Return from interrupt and sets the IME."
	    :opcode #b11011001
	    :cycles 4)
     (alist :jump? t)
     (pop-instr-spec :pc)
     (alist :ime? t)))

   ;; RET cc
   (map-jump-conditional-opcodes
    (lambda (opcode condition-key)
      (merge-instr-specs
       (instr-spec-defaults
	:name (list :ret condition-key)
	:description (format nil "If ~A, POP the value off the stack into the PC.
Otherwise increment PC to the next instruction."
			     (condition-key-description condition-key))
	:opcode opcode)
       (condition?-bindings condition-key)
       (alist :bindings `((sp (cpu-sp cpu))
			  (sp+2 (+ 2 sp)))
	      :sp `(if condition? sp+2 sp)
	      :cycles '(if condition? 5 2))
       (pc-jump (compile-16bit-memory-access 'sp)
		:condition-form 'condition?
		:instr-size 1)))
    #b11000000)

   ;; RST t
   (map-opcodes
    (lambda (opcode index)
      (merge-instr-specs
       (instr-spec-defaults
	:name (list :rst-t :addr)
	:description "Jump to a predefined address based on t (0-7): (#x0000 #x0008 #x0010 #x0018 #x0020 #x0028 #x0030 #x0038)."
	:opcode opcode
	:cycles 4
	:disassembly (alist :t index))
       (push-value-instr-spec '(1+ pc))
       (pc-jump (* index 8) :instr-size 1)))
    #b11000111 (alist 3 '(0 1 2 3 4 5 6 7)))))

;; TODO: Some way to leave memory unchanged?

(defun rotate-left (register carry-bit)
  (logior carry-bit (logand #xff (ash register 1))))

(defun instr-effects (cpu memory)
  (let* ((instr (next-instr cpu memory)))
    (when instr
      (funcall (aval :instr-effects instr) cpu memory))))

(defun bit-value (value index)
  (let ((bitmask (ash 1 index)))
    (if (zerop (logand bitmask value))
	0
	1)))

(defun daa-carry? (subtraction? carry? half-carry? byte)
  (and (not subtraction?)
       (or carry?
	   half-carry?
	   (> byte #x99)
	   (> (lo-nibble byte) #x09))))
(defun daa (subtraction? carry? half-carry? byte)
  (let* ((lo (lo-nibble byte))
	 (adjustment 0))
    (cond
      (subtraction?
       (when carry? (decf adjustment #x60))
       (when half-carry? (decf adjustment #x06)))
      (t
       (when (or carry? (> byte #x99)) (incf adjustment #x60))
       (when (or half-carry? (> lo #x09)) (incf adjustment #x06))))
    (+ byte adjustment)))

;; S2.9
(defun general-purpose-arithmetic-instr-specs ()
  (append
   (list
    ;; DAA
    (merge-instr-specs
     (alist
      :name :daa
      :description "Performs post-arithmetic adjustment for the binary coded decimal value in A."
      :opcode #b00100111
      :cycles 1)
     (a-binding)
     (alist :bindings '((n? (cpu-subtraction? cpu))
			(cy? (cpu-carry? cpu))
			(h? (cpu-half-carry? cpu))))
     (result-bindings '(daa n? cy? h? a))
     (alist
      :a *result-name*
      :carry? '(daa-carry? n? cy? h? a)
      :zero? '(zerop result)
      :half-carry? nil)
     (pc-next-instr))
    
    ;; CPL
    (merge-instr-specs
     (alist
      :name :cpl
      :opcode #b00101111
      :description "Takes the one's complement of the contents of register A."
      :cycles 1
      :half-carry? t
      :subtraction? t)
     (pc-next-instr)
     (a-binding)
     (result-bindings `(logxor #xff ,*a-name*))
     (key-set-instr-spec :a *result-name*))

    ;; NOP
    (merge-instr-specs
     (alist
      :name :nop
      :opcode #b00000000
      :description "Advances the PC by 1 (no-op)."
      :cycles 1)
     (pc-next-instr))

    ;; CCF
    (merge-instr-specs
     (alist
      :name :ccf
      :opcode #b00111111
      :description "Stores the complement of the carry flag in the carry flag."

      :cycles 1
      :half-carry? nil
      :subtraction? nil
      :carry? `(not (cpu-carry? ,*cpu-name*)))
     (pc-next-instr))

    ;; SCF
    (merge-instr-specs
     (alist
      :name :scf
      :cycles 1
      :opcode #b00110111
      :description "Sets the carry flag to 1."

      :half-carry? nil
      :subtraction? nil
      :carry? t)
     (pc-next-instr))

    ;; DI
    (merge-instr-specs
     (alist
      :name :di
      :opcode #b11110011
      :description "Disables maskable interrupts. Sets IME to 0."
      :cycles 1)
     (alist :ime? nil)
     (pc-next-instr))
    
    ;; EI
    (merge-instr-specs
     (alist
      :name :ei
      :opcode #b11111011
      :description "Enables maskable interrupts. Sets IME to 1."
      :cycles 1)
     (alist :ime? t)
     (pc-next-instr))

    ;; HALT: TODO
    (merge-instr-specs
     (alist :name :halt
	    :opcode #b01110110
	    :description "Stops the system clock. Waits for interrupt request flag and corresponding interrupt enable flag to be set
before resuming execution."
	    :cycles 1)
     (pc-next-instr))

    ;; STOP: TODO
    (merge-instr-specs
     (alist :name :STOP
	    :opcode #b00010000
	    :description "Stops the system clock. Stops the oscillator circuit. 
Waits for a reset signal."
	    :cycles 1)
     (pc-next-instr :instr-size 2)))))


(defvar *breakpoints* ())
(defmacro defbreakpoint (name (cpu memory) &body body)
  `(asetq ',name (lambda (,cpu ,memory)
		   (declare (ignorable ,cpu ,memory))
		   ,@body)
	  *breakpoints*))
(defmacro undefbreakpoint (name &rest ignored)
  (declare (ignore ignored))
  `(setq *breakpoints* (aremove *breakpoints* ',name)))

(defun break? (cpu memory)
  (some (fn (funcall % cpu memory)) (mapcar 'cdr *breakpoints*)))

(undefbreakpoint break-out-of-loop (cpu memory)
		 (let* ((pc (cpu-pc cpu)))
		   (> pc #xA)))

(undefbreakpoint copy-nintendo-loop (cpu memory)
		 (= (cpu-pc cpu) #x2b))

;; TODO: label modes with defparameters
(undefbreakpoint vblank-start (cpu memory)
		 (and *lcd-start-cycle*
		      (not
		       (eql (lcd-mode) (lcd-mode-from-cycles (- *current-cycle*
								*lcd-start-cycle*)
							     0)))
		      (eql 1 (lcd-mode-from-cycles (- *current-cycle*
						      *lcd-start-cycle*)
						   0))))

(undefbreakpoint wait-loop (cpu memory)
		 (let* ((pc (cpu-pc cpu)))
		   (= #x64 pc)))

(defun compile-disassemble-instr-spec (instr-spec)
  (compile-disassemble-instr
   (aval :bindings instr-spec)
   (aval :disassembly instr-spec)))

(defun define-instrs! ()
  ;; Define instr-effects and disassemble-instr for each instr-spec
  (setq *instr-specs*
	(mapcar (fn
		  (amerge %
			  (alist :instr-effects (eval (compile-instr-spec-effects %))
				 :disassemble-instr (eval (compile-disassemble-instr-spec %)))))
		(append
		 (ld-8bit-instr-specs)
		 (ld-16bit-instr-specs)
		 (8bit-alu-instr-specs)
		 (16bit-alu-op-instr-specs)
		 (rotate-shift-instr-specs)
		 (bit-instr-specs)
		 (jump-instr-specs)
		 (call-and-return-instr-specs)
		 (general-purpose-arithmetic-instr-specs))))
  
  ;; TEMP: Add all single-byte instr-specs to instrs
  (mapcar (fn (asetq (aval :opcode %) % *instrs*)) (remove-if (fn (aval :long? %)) *instr-specs*))
  ;; TEMP: add all 2-byte instr-specs to instrs
  (mapcar (fn (asetq (aval :opcode %) % *long-instrs*)) (remove-if-not (fn (aval :long? %)) *instr-specs*))
  ;; Recompile the execute! function
  (eval (compile-execute-next-instr))
  (eval `(defun interrupt! (addr cpu memory)
	   ,(compile-instr-spec-for-execute
	     (merge-instr-specs
	      (alist :cycles 5)
	      (push-instr-spec :pc)
	      (pc-jump 'addr))))))

;; recompile execute-next-instr! definition
(define-instrs!)

(defreset-var *current-cycle* 0)
(defreset-var *lcd-start-cycle* nil)
(defun execute! (cpu memory)
  (cond
    ((lcdc-display?)
     (when (null *lcd-start-cycle*)
       ;; start lcd
       (setq *lcd-start-cycle* *current-cycle*))
     (update-lcd! *lcd-start-cycle* *current-cycle*))
    (t
     ;; TODO: should this be reset or should it be restored?
     ;;(setq *lcd-start-cycle* nil)
     ;;(set-lcd-scanline! 0)
     ))

  (incf *current-cycle*
	(if (cpu-ime? cpu)
	    (let* ((interrupt-flags (mem-byte #xff0f memory))
		   (interrupt-enable (mem-byte #xffff memory))
		   (interrupts (logand interrupt-flags interrupt-enable)))
	      (cond
		((bit-set? 0 interrupts)
		 (mem-write! #xff0f (bit-set 0 interrupt-flags 0) memory)
		 (interrupt! #x0040 cpu memory))
		((bit-set? 1 interrupts)
		 (mem-write! #xff0f (bit-set 1 interrupt-flags 0) memory)
		 (interrupt! #x0048 cpu memory))
		((bit-set? 2 interrupts)
		 (mem-write! #xff0f (bit-set 2 interrupt-flags 0) memory)
		 (interrupt! #x0050 cpu memory))
		((bit-set? 3 interrupts)
		 (mem-write! #xff0f (bit-set 3 interrupt-flags 0) memory)
		 (interrupt! #x0058 cpu memory))
		((bit-set? 4 interrupts)
		 (mem-write! #xff0f (bit-set 4 interrupt-flags 0) memory)
		 (interrupt! #x0060 cpu memory))
		(t (execute-next-instr! cpu memory))))
	    (execute-next-instr! cpu memory))))

(defun remove-newlines (string)
  (map 'string (fn (if (eql #\newline %)
		       #\space
		       %))
       string))

(defun split-line (string line-length)
  (if (<= (length string) line-length)
      (list string)
      (let* ((split-index (position #\space string :from-end t :end line-length)))
	(if split-index
	    (list (subseq string 0 split-index)
		  (subseq string (1+ split-index)))
	    (let* ((split-index (position #\space string :start line-length)))
	      (if split-index
		  (list (subseq string 0 split-index)
			(subseq string (1+ split-index)))
		  (list string)))))))

(defun word-wrap (string line-length)
  (let* ((lines (split-line string line-length))
	 (first (first lines))
	 (second (second lines)))
    (if second
	(cons first (word-wrap second line-length))
	lines)))

;; TODO: visualize interrupt registers
;; TODO: visualize control registers

;; TODO: focus on the memory address that was last modified

(defun draw-color-palette! (pos title-texture-id palette-addr sprite-palette?)
  (draw-full-texture-id-right-aligned! title-texture-id pos)
  (loop for color in (palette-colors-from-color-palette-addr palette-addr)
	for i from (if sprite-palette? 1 0) below 4 do
	  (set-color! color)
	  (fill-rect! (+ (g1 i) (x pos)) (y pos) (g1 1) (g1 1))
	  (set-color! (red 50))
	  (draw-rect! (+ (g1 i) (x pos)) (y pos) (g1 1) (g1 1))))

(defun palette-colors-from-color-palette-addr (palette-addr)
  (palette-colors-from-color-palette-byte (mem-byte palette-addr)))

(defun palette-color-index (byte index)
  (logand #b11 (ash byte (- (* 2 index)))))

(defun palette-color (byte index)
  (ecase (palette-color-index byte index)
    (0 (green 200))
    (1 (green 150))
    (2 (green 100))
    (3 (green 50))))


(defun palette-colors-from-color-palette-byte (byte)
  (loop for i below 4 collecting (palette-color byte i)))

(defun initialize-color-palette! (title pos palette-addr sprite-palette?)
  (let* ((title-texture-id (gensym)))
    (load-text-texture! title-texture-id :font title)
    (alist :pos pos
	   :title-texture-id title-texture-id
	   :palette-addr palette-addr
	   :sprite-palette? sprite-palette?)))

(defvar *color-palettes* ())

(defparameter *bg-color-palette-addr* #xff47)
(defun initialize-color-palettes! ()
  (setq *color-palettes* (list (initialize-color-palette! "BG" (g2 3 1) *bg-color-palette-addr* nil)
			       (initialize-color-palette! "OBJ0" (g2 3 2) #xff48 t)
			       (initialize-color-palette! "OBJ1" (g2 3 3) #xff49 t))))

(defun draw-color-palettes! ()
  (mapcar 
   (fn (draw-color-palette! (aval :pos %)
			    (aval :title-texture-id %)
			    (aval :palette-addr %)
			    (aval :sprite-palette? %)))
   *color-palettes*))

;; Tile data block: 16x8 tiles

;; 16x8 8x8 textures

(defun initialize-tile-data-block-textures! ()
  (let* ((textures (make-array 128)))
    (loop for i below (length textures) do
      (setf (aref textures i) (create-pixel-buffer-texture! 8 8)))
    textures))

(defun tile-data-row-color-indices (hi-byte lo-byte)
  (loop for i from 7 downto 0
	collecting
	(logior (ash (bit-value hi-byte i) 1)
		(bit-value lo-byte i))))

(defun tile-data-color-indices (tile-data)
  ;; Tile data is 8 rows of 2-bytes
  (loop for i below 8
	appending (tile-data-row-color-indices
		   (aref tile-data (1+ (* 2 i)))
		   (aref tile-data (* 2 i)))))

(defun tile-data-colors (tile-data palette-colors sprite?)
  (mapcar (fn (if (and (zerop %) sprite?)
		  (color 255 0 255 255)
		  (nth % palette-colors)))
	  (tile-data-color-indices tile-data)))

(defun rgba-pixels-from-colors (colors)
  (let* ((rgba-pixels (make-array (list (* 4 (length colors))) :element-type '(unsigned-byte 8))))
    (loop for i below (length rgba-pixels)
	  for color in colors do
	    (setf (aref rgba-pixels (+ 0 (* i 4))) (r color)
		  (aref rgba-pixels (+ 1 (* i 4))) (g color)
		  (aref rgba-pixels (+ 2 (* i 4))) (b color)
		  (aref rgba-pixels (+ 3 (* i 4))) (a color)))
    rgba-pixels))

(defun fill-tile-data-texture! (tile-texture tile-data palette-colors &optional sprite?)
  (replace-pixel-buffer! tile-texture
			 (rgba-pixels-from-colors
			  (tile-data-colors tile-data palette-colors sprite?))))

(defun tile-data-from-block (tile-data-block index)
  (let* ((start (* index 16)))
    (subseq tile-data-block start (+ start 16))))

(defun fill-tile-data-textures! (tile-data-block)
  (ensure-tile-textures!)
  (let* ((palette-colors (default-palette-colors)))
    (loop for i below (length *tile-textures*) do
      (fill-tile-data-texture! (aref *tile-textures* i)
			       (tile-data-from-block tile-data-block i)
			       palette-colors))))

(defun tile-data-block-size ()
  (* 16 128))
(defun tile-data-block0 ()
  (subseq *memory* #x8000 (+ #x8000 (tile-data-block-size))))
(defun tile-data-block1 ()
  (subseq *memory* #x8800 (+ #x8800 (tile-data-block-size))))
(defun tile-data-block2 ()
  (subseq *memory* #x9000 (+ #x9000 (tile-data-block-size))))

(defvar *tile-textures* nil)

(defun draw-tile-data-texture! (pos tile-texture)
  (draw-texture! tile-texture
		 0 0
		 (texture-width tile-texture) (texture-height tile-texture)
		 (1+ (x pos)) (1+ (y pos)) (- (g1 1) 2) (- (g1 1) 2)))

(defun draw-tile-data-textures! (pos)
  (loop for x below 16 do
    (loop for y below 8 do
      (draw-tile-data-texture! (v+ pos (g2 x y))
			       (aref *tile-textures* (+ x (* y 16)))))))

(defun default-palette-colors ()
  (palette-colors-from-color-palette-byte
   #b11101000))

(defun ensure-tile-textures! ()
  (unless *tile-textures*
    (setq *tile-textures* (initialize-tile-data-block-textures!))))

(defun initialize-block-visualization! ()
  (ecase *lcd-visualization-current-tile-data-block*
    (0 (fill-tile-data-textures! (tile-data-block0)))
    (1 (fill-tile-data-textures! (tile-data-block1)))
    (2 (fill-tile-data-textures! (tile-data-block2)))))


;; Bg Tile Map
;; 32x32
(defvar *tile-map-texture* nil)

(defun tile-addresses (tile-map 8000-addressing-mode?)
  (loop for tile-byte across tile-map
	collecting
	(if 8000-addressing-mode?
	    (+ (* tile-byte 16) #x8000)
	    (+ (* 16 (s8 tile-byte)) #x8800))))

(defun copy-tile-map-pixels-from-tile-data! (rgba-pixels tile-map-index tile-data-colors)
  (loop for tile-data-pixel-x below 8 do
    (loop for tile-data-pixel-y below 8 do
      (let* ((tiles/tile-map-row 32)
	     (pixels/tile-row 8)

	     (tile-map-x (mod tile-map-index tiles/tile-map-row))
	     (tile-map-y (truncate tile-map-index tiles/tile-map-row))

	     (tile-map-pixel-x (* pixels/tile-row tile-map-x))
	     (tile-map-pixel-y (* pixels/tile-row tile-map-y))

	     (pixel-x (+ tile-data-pixel-x tile-map-pixel-x))
	     (pixel-y (+ tile-data-pixel-y tile-map-pixel-y))

	     (pixels/tile-map-row (* tiles/tile-map-row pixels/tile-row))

	     (pixel-index (+ (* pixel-y pixels/tile-map-row) pixel-x))
	     (byte-index (* 4 pixel-index))

	     (tile-data-pixel-index (+ tile-data-pixel-x (* tile-data-pixel-y pixels/tile-row)))
	     (color (nth tile-data-pixel-index tile-data-colors)))
	(setf (aref rgba-pixels (+ 0 byte-index)) (r color)
	      (aref rgba-pixels (+ 1 byte-index)) (g color)
	      (aref rgba-pixels (+ 2 byte-index)) (b color)
	      (aref rgba-pixels (+ 3 byte-index)) (a color))))))

(defun fill-tile-map-texture! (tile-map 8000-addressing-mode?)
  (let* ((tile-addresses (tile-addresses tile-map 8000-addressing-mode?))
	 (rgba-pixels (make-array (list (* 4 8 8 32 32)) :element-type '(unsigned-byte 8))))
    (loop for addr in tile-addresses
	  for i from 0 do
	    (let* ((tile-data (subseq *memory* addr (+ addr 16)))
		   (colors (tile-data-colors tile-data (default-palette-colors) nil)))
	      (copy-tile-map-pixels-from-tile-data! rgba-pixels i colors)))
    (replace-pixel-buffer! *tile-map-texture* rgba-pixels)))

(defun lcdc-register ()
  (mem-byte #xff40))
(defun 8000-addressing-mode? ()
  (bit-set? 4 (lcdc-register)))
(defun scroll-x ()
  (mem-byte #xff43))
(defun scroll-y ()
  (mem-byte #xff42))
(defun object-size-doubled? ()
  (bit-set? 2 (lcdc-register)))
(defun window-x ()
  (+ (mem-byte #xff4b) 7))
(defun window-y ()
  (mem-byte #xff4a))
(defun lcd-scanline ()
  (mem-byte #xff44))
(defun set-lcd-scanline! (val)
  (mem-write! #xff44 val))
(defun lcd-scanline-compare ()
  (mem-byte #xff45))
(defun lcds-register ()
  (mem-byte #xff41))
(defun lcd-mode ()
  (logand #b11 (lcds-register)))
(defun set-lcd-mode! (mode)
  (mem-write! #xff41 (logior mode (logand #b11111100 (lcds-register)))))
(defun lcd-dma-start ()
  (ash (mem-byte #xff46) 4))
(defun lcd-coincidence? ()
  (bit-set? 2 (lcds-register)))
(defun lcdc-background-tiles-addr ()
  (if (bit-set? 3 (lcdc-register))
      #x9C00
      #x9800))
(defun lcdc-window-tiles-addr ()
  (if (bit-set? 6 (lcdc-register))
      #x9C00
      #x9800))
(defun lcdc-display? ()
  (bit-set? 7 (lcdc-register)))
(defun lcdc-window-display? ()
  (bit-set? 5 (lcdc-register)))
(defun lcdc-object-display? ()
  (bit-set? 1 (lcdc-register)))
(defun lcdc-background/window-display? ()
  (bit-set? 0 (lcdc-register)))

(defun lcds-coincidence-interrupt-enabled? ()
  (bit-set? 6 (lcds-register)))
(defun lcds-oam-interrupt-enabled? ()
  (bit-set? 5 (lcds-register)))
(defun lcds-v-blank-interrupt-enabled? ()
  (bit-set? 4 (lcds-register)))
(defun lcds-h-blank-interrupt-enabled? ()
  (bit-set? 3 (lcds-register)))

(defun background-tile-map ()
  (let* ((addr (lcdc-background-tiles-addr)))
    (subseq *memory* addr (+ addr #x0400))))
(defun window-tile-map ()
  (let* ((addr (lcdc-window-tiles-addr)))
    (subseq *memory* addr (+ addr #x0400))))

(defun ensure-tile-map-texture! ()
  (setq *tile-map-texture* (create-pixel-buffer-texture! 256 256)))


(defvar *draw-background-tile-map?* nil)
(defun initialize-background-visualization! ()
  (let* ((tile-map (if *draw-background-tile-map?*
		       (background-tile-map)
		       (window-tile-map))))
    (ensure-tile-map-texture!)
    (fill-tile-map-texture! tile-map (8000-addressing-mode?))
    (load-text-texture! :tile-map-pos :font (if *draw-background-tile-map?*
						(format nil "<~A, ~A>" (scroll-x) (scroll-y))
						(format nil "<~A, ~A>" (window-x) (window-y))))))

(defun ensure-pixel-buffer-texture-loaded! (id width height)
  (unless (gethash id *textures*)
    ;; Create the texture and add it to the textures hash-table
    (setf (gethash id *textures*) (create-pixel-buffer-texture! width height))
    (notify-handlers! (event-texture-loaded id))))

(defun draw-sprite-texture! (tile-index palette-colors dest-pos)
  (ensure-pixel-buffer-texture-loaded! :sprite-texture 8 8)
  (let* ((sprite-texture (gethash :sprite-texture *textures*))
	 (tile-data (if (> tile-index 127)
			(tile-data-from-block (tile-data-block1) (- tile-index 128))
			(tile-data-from-block (tile-data-block0) tile-index))))
    (fill-tile-data-texture! sprite-texture tile-data palette-colors t)
    (draw-texture! sprite-texture 0 0 8 8 (x dest-pos) (y dest-pos) (g1 1) (g1 1))))

(defun draw-sprites! (pos)
  (let* ((object-size-doubled? (object-size-doubled?)))
    (loop for x below 8 do
      (loop for y below 5 do
	(let* ((sprite-index (+ x (* 8 y)))
	       (sprite-addr (+ #xfe00 (* 4 sprite-index)))

	       (tile/pattern-number (mem-byte (+ sprite-addr 2)))
	       (flags (mem-byte (+ sprite-addr 3)))
	       (palette-index1? (bit-set? 4 flags))
	       (palette-colors (palette-colors-from-color-palette-addr
				(if palette-index1?
				    #xff49
				    #xff48))))
	  (cond
	    (object-size-doubled?
	     (draw-sprite-texture! (logand #xfe tile/pattern-number) palette-colors
				   (v+ pos (g2 x (* 2 y))))
	     (draw-sprite-texture! (logior #x01 tile/pattern-number) palette-colors
				   (v+ pos (g2 x (1+ (* 2 y))))))
	    (t
	     (draw-sprite-texture! tile/pattern-number palette-colors
				   (v+ pos (g2 x y))))))))))

(defun display-memory-visualiztions! ()
  (mapcar
   'event!
   (list
    (alist :type :hide :id :main-panel)
    (alist :type :display :id :memory-visualizations))))
(defun display-lcd-visualiztions! ()
  (mapcar
   'event!
   (list
    (alist :type :hide :id :main-panel)
    (alist :type :display :id :lcd-visualizations))))

(defbutton memory (button "Memory" 1 (g2 1 28) :font)
  (unless (draw-memory-visualizations?)
    (display-memory-visualiztions!)))
(defbutton lcd-debug (button "LCD Debug" 1 (g2 5 28) :font)
  (when (not (eql :lcd-debug *main-panel*))
    (display-lcd-visualiztions!)))
(defbutton game (button "Game" 1 (g2 10 28) :font)
  (when (not (eql :game *main-panel*))
    (mapcar
     'event!
     (list
      (alist :type :hide :id :main-panel)
      (alist :type :display :id :game)))))

(defun button-spec (id button on-click-fn)
  "A button spec used for create-button!, uses a button spec for defbutton."
  (amerge button (alist :id id :on-click-fn on-click-fn)))

(defun register-button-handler! (button)
  (register-handler! (aval :click-handler-id button)
		     (button-clicked-event-handler (aval :id button)
						   (aval :on-click-fn button))))

(defun update-button! (old-button button)
  (let* ((new-button (amerge old-button button)))
    (register-button-handler! new-button)
    (load-text-texture! (aval :texture-id new-button)
			(aval :font-id new-button)
			(aval :text new-button))
    (set-button! (aval :id button) new-button)))

(defun button-clicked-event-handler (button-id on-click-fn)
  (event-handler
   (EVENT-MATCHER-BUTTON-CLICKED button-id)
   on-click-fn))

(defun create-button! (button)
  (let* ((id (aval :id button))
	 (old-button (get-button id)))
    (cond
      (old-button (update-button! old-button button))
      (t
       (let* ((button (aset :click-handler-id (gensym) button)))
	 (register-button-handler! button)
	 (set-button! id button)
	 (initialize-button! id))))
    id))

(defvar *lcd-visualization-current-tile-data-block* 0)

(defun lcd-buttons ()
  (list
   (button-spec
    :tile-data-block0
    (button "Blk0" 1 (g2 3 13) :font)
    (fn
      (setq *lcd-visualization-current-tile-data-block* 0)
      (initialize-block-visualization!)))
   (button-spec
    :tile-data-block1
    (button "Blk1" 1 (g2 6 13) :font)
    (fn
      (setq *lcd-visualization-current-tile-data-block* 1)
      (initialize-block-visualization!)))
   (button-spec
    :tile-data-block2
    (button "Blk2" 1 (g2 9 13) :font)
    (fn
      (setq *lcd-visualization-current-tile-data-block* 2)
      (initialize-block-visualization!)))
   
   (button-spec
    :background
    (button "BG" 1 (g2 3 27) :font)
    (fn
      (setq *draw-background-tile-map?* t)
      (initialize-background-visualization!)
      (add-drawing! :background-scroll (background-scroll-drawing))))
   (button-spec
    :window
    (button "Window" 1 (g2 5 27) :font)
    (fn
      (setq *draw-background-tile-map?* nil)
      (initialize-background-visualization!)
      (remove-drawing! :background-scroll)))))

(defun event-display-matcher (id)
  (fn (and (eql (aval :type %) :display)
	   (eql (aval :id %) id))))
(defun event-hide-matcher (id)
  (fn (and (eql (aval :type %) :hide)
	   (eql (aval :id %) id))))


(defun update-lcd-visualizations! ()
  (unless (not (eql *main-panel* :lcd-debug))
    (initialize-color-palettes!)
    (initialize-background-visualization!)
    (initialize-block-visualization!)
    (load-text-texture! :lcd-status-scanline :font "Scanline: ")
    (load-text-texture! :lcd-status-scanline-data :font (u8-text (lcd-scanline)))
    (load-text-texture! :lcd-status-compare-scanline :font "CmpScanline: ")
    (load-text-texture! :lcd-status-compare-scanline-data :font (u8-text (lcd-scanline-compare)))
    (load-text-texture! :lcd-status-mode :font "Mode: ")
    (load-text-texture! :lcd-status-mode-data :font (let* ((mode (lcd-mode)))
						      (cond
							((= mode *h-blank-mode*) "H-Blank")
							((= mode *v-blank-mode*) "V-Blank")
							((= mode *oam-mode*) "OAM")
							((= mode *vram-mode*) "OAM/VRAM"))))
    (load-text-texture! :lcd-status-coincidence? :font "Coincidence?: ")
    (load-text-texture! :lcd-status-dma-address :font "DMA Start: ")
    (load-text-texture! :lcd-status-dma-address-data :font (hex16-text (lcd-dma-start)))

    (load-text-texture! :lcdc-display? :font "Display?: ")
    (load-text-texture! :lcdc-window-tiles :font "Window Tiles: ")
    (load-text-texture! :lcdc-window-tiles-data :font (hex16-text (lcdc-window-tiles-addr)))
    (load-text-texture! :lcdc-window-disp? :font "Window Disp?: ")
    (load-text-texture! :lcdc-bg-tiles :font "BG Tiles: ")
    (load-text-texture! :lcdc-bg-tiles-data :font (hex16-text (lcdc-background-tiles-addr)))
    (load-text-texture! :lcdc-object-size :font "Object Size: ")
    (load-text-texture! :lcdc-object-size-data :font (if (object-size-doubled?)
							 "2x"
							 "1x"))
    (load-text-texture! :lcdc-object-display? :font "Object Disp?: ")
    (load-text-texture! :lcdc-background/window-display? :font "BG/Win Disp?: ")))

(defun display-event-handler (display-id fn)
  (event-handler
   (event-display-matcher display-id)
   (fn (funcall fn))))
(defun hide-event-handler (panel-id fn)
  (event-handler
   (event-hide-matcher panel-id)
   (fn
     (funcall fn))))

(defvisualization :lcd-debug
    (alist :initialization
	   (fn (case *main-panel*
		 (:memory (display-memory-visualiztions!))
		 (:lcd-debug (display-lcd-visualiztions!))))
	   :update
	   (fn (update-lcd-visualizations!))
	   :display-event-handler
	   (display-event-handler
	    :lcd-visualizations
	    (fn
	      (setq *main-panel* :lcd-debug)
	      (update-lcd-visualizations!)

	      (mapcar 'create-button! (lcd-buttons))
	      (add-drawing! :lcd-visualizations (drawing 1 (fn (draw-lcd-visualizations!))))
	      (add-drawing! :background-scroll (background-scroll-drawing))))
	   :hide-event-handler
	   (hide-event-handler
	    :main-panel
	    (fn
	      (mapcar (fn (remove-button! (aval :id %))) (lcd-buttons))
	      (mapcar 'remove-drawing! '(:lcd-visualizations :background-scroll))))))

(defun draw-lcd-visualizations! ()
  (draw-sprites! (g2 16 14))
  (draw-tile-data-textures! (g2 3 5))
  (draw-color-palettes!)
  (draw-full-texture! *tile-map-texture* (g1 3) (g1 14))
  (draw-full-texture-id! :tile-map-pos (g2 10 27))

  ;; TODO: Modulate colors based on what has/hasn't changed.
  (let* ((column 27)
	 (y 6))
    (draw-full-texture-id-right-aligned! :lcdc-display? (g2 column y))
    (draw-full-texture-id! (if (lcdc-display?) :yes :no) (g2 column y))

    (incf y)
    (draw-full-texture-id-right-aligned! :lcdc-background/window-display? (g2 column y))
    (draw-full-texture-id! (if (lcdc-background/window-display?) :yes :no) (g2 column y))

    (incf y)
    (draw-full-texture-id-right-aligned! :lcdc-window-disp? (g2 column y))
    (draw-full-texture-id! (if (lcdc-window-display?) :yes :no) (g2 column y))

    (incf y)
    (draw-full-texture-id-right-aligned! :lcdc-object-display? (g2 column y))
    (draw-full-texture-id! (if (lcdc-object-display?) :yes :no) (g2 column y))

    (incf y)
    (draw-full-texture-id-right-aligned! :lcdc-window-tiles (g2 column y))
    (draw-full-texture-id! :lcdc-window-tiles-data (g2 column y))
    
    (incf y)
    (draw-full-texture-id-right-aligned! :lcdc-bg-tiles (g2 column y))
    (draw-full-texture-id! :lcdc-bg-tiles-data (g2 column y))

    (incf y)
    (draw-full-texture-id-right-aligned! :lcdc-object-size (g2 column y))
    (draw-full-texture-id! :lcdc-object-size-data (g2 column y)))

  (let* ((column 24)
	 (y 21))
    (draw-full-texture-id-right-aligned! :lcd-status-scanline (g2 column y))
    (draw-full-texture-id! :lcd-status-scanline-data (g2 column y))
    (incf y)
    (draw-full-texture-id-right-aligned! :lcd-status-compare-scanline (g2 column y))
    (draw-full-texture-id! :lcd-status-compare-scanline-data (g2 column y))
    (incf y)
    (draw-full-texture-id-right-aligned! :lcd-status-mode (g2 column y))
    (draw-full-texture-id! :lcd-status-mode-data (g2 column y))
    (incf y)
    (draw-full-texture-id-right-aligned! :lcd-status-coincidence? (g2 column y))
    (draw-full-texture-id! (if (lcd-coincidence?) :yes :no) (g2 column y))
    (incf y)
    (draw-full-texture-id-right-aligned! :lcd-status-dma-address (g2 column y))
    (draw-full-texture-id! :lcd-status-dma-address-data (g2 column y))))

(defun background-scroll-drawing ()
  (drawing 3 (fn
	       (set-color! (blue))
	       (draw-rect! (+ (g1 3) (scroll-x))
			   (+ (g1 14) (scroll-y))
			   160 144))))

;; Display/Hide Memory Visualizations (when toggling memory/lcd vis)
(defhandler handle-display-memory-visualizations (event)
	    (event-display-matcher :memory-visualizations)
  (setq *main-panel* :memory)
  (amap 'initialize-memory-visualization! *memory-visualizations*)
  (add-drawing! :memory-visualizations (drawing 1 (fn (draw-memory-visualizations!)))))
(defhandler handle-hide-memory-visualizations (event)
	    (event-hide-matcher :main-panel)
  (remove-drawing! :memory-visualizations))

;; TODO: buttons should have ids on them

(defun interrupt-enable-register ()
  (mem-byte #xffff))
(defun interrupt-v-blank-enabled? ()
  (bit-set? 0 (interrupt-enable-register)))
(defun set-v-blank-interrupt! (&optional (bit-value 1))
  (mem-write! #xffff
	      (bit-set 0 (interrupt-enable-register) bit-value)))
(defun interrupt-lcd-enabled? ()
  (bit-set? 1 (interrupt-enable-register)))
(defun set-lcd-interrupt! (&optional (bit-value 1))
  (mem-write! #xffff
	      (bit-set 1 (interrupt-enable-register) bit-value)))
(defun interrupt-timer-enabled? ()
  (bit-set? 2 (interrupt-enable-register)))
(defun interrupt-serial-enabled? ()
  (bit-set? 3 (interrupt-enable-register)))
(defun interrupt-joypad-enabled? ()
  (bit-set? 4 (interrupt-enable-register)))

(defun draw-interrupt-enabled-visualization! ()
  (let* ((column 59)
	 (y 2))
    (draw-full-texture-id-right-aligned! :interrupt-v-blank (g2 column y))
    (draw-full-texture-id! (if (interrupt-v-blank-enabled?) :yes :no) (g2 column y))

    (incf y)
    (draw-full-texture-id-right-aligned! :interrupt-timer (g2 column y))
    (draw-full-texture-id! (if (interrupt-timer-enabled?) :yes :no) (g2 column y))

    (incf y)
    (draw-full-texture-id-right-aligned! :interrupt-serial (g2 column y))
    (draw-full-texture-id! (if (interrupt-timer-enabled?) :yes :no) (g2 column y))

    (incf y)
    (draw-full-texture-id-right-aligned! :interrupt-joypad (g2 column y))
    (draw-full-texture-id! (if (interrupt-joypad-enabled?) :yes :no) (g2 column y))

    (incf y)
    (incf y)
    (draw-full-texture-id-right-aligned! :interrupt-lcd (g2 column y))
    (draw-full-texture-id! (if (interrupt-lcd-enabled?) :yes :no) (g2 column y))

    (incf y)
    (draw-full-texture-id-right-aligned! :interrupt-lcd-coincidence (g2 column y))
    (draw-full-texture-id! (if (lcds-coincidence-interrupt-enabled?) :yes :no) (g2 column y))

    (incf y)
    (draw-full-texture-id-right-aligned! :interrupt-lcd-oam (g2 column y))
    (draw-full-texture-id! (if (lcds-oam-interrupt-enabled?) :yes :no) (g2 column y))

    (incf y)
    (draw-full-texture-id-right-aligned! :interrupt-lcd-v-blank (g2 column y))
    (draw-full-texture-id! (if (lcds-v-blank-interrupt-enabled?) :yes :no) (g2 column y))

    (incf y)
    (draw-full-texture-id-right-aligned! :interrupt-lcd-h-blank (g2 column y))
    (draw-full-texture-id! (if (lcds-h-blank-interrupt-enabled?) :yes :no) (g2 column y))))

(defmacro definit (name &body body)
  `(defhandler ,name (event) (event-matcher-initialization-finished)
     ,@body))

(definit handle-initialize-interrupt-enabled-visualization
  (load-text-texture! :interrupt-v-blank :font "V-Blank: ")
  (load-text-texture! :interrupt-lcd :font "LCD: ")
  (load-text-texture! :interrupt-timer :font "Timer: ")
  (load-text-texture! :interrupt-serial :font "Serial: ")
  (load-text-texture! :interrupt-joypad :font "Joypad: ")
  
  (load-text-texture! :interrupt-lcd-coincidence :font "Coincidence: ")
  (load-text-texture! :interrupt-lcd-oam :font "OAM: ")
  (load-text-texture! :interrupt-lcd-v-blank :font "V-Blank: ")
  (load-text-texture! :interrupt-lcd-h-blank :font "H-Blank: ")
  (add-drawing! :interrupts-enabled (drawing 3 (fn (draw-interrupt-enabled-visualization!)))))


(defun mode0-dots (num-sprites)
  (- 376 (mode3-dots num-sprites)))
(defun mode1-dots ()
  4560)
(defun mode2-dots ()
  80)
(defun mode3-dots (num-sprites)
  (ecase num-sprites
    (0 168)
    (1 180)
    (2 193)

    (3 205)
    (4 217)
    (5 229)

    (6 242)
    (7 254)
    (8 266)

    (9 279)
    (10 291)))

(defun lcd-scanline-dots ()
  (+ (mode0-dots 0)
     (mode2-dots)
     (mode3-dots 0)))
(defun lcd-dots ()
  (+ (mode1-dots)
     (* 144 (lcd-scanline-dots))))

(defun lcd-scanline-from-cycles (elapsed-cycles num-sprites)
  (let* ((elapsed-dots (* 4 elapsed-cycles))
	 (dot (mod elapsed-dots (lcd-dots)))
	 (mode0-dots (mode0-dots num-sprites)))
    (cond
      ((< dot mode0-dots) 143)
      (t (mod (+ 144 (truncate (- dot mode0-dots) (lcd-scanline-dots))) 154)))))

(defparameter *h-blank-mode* 0)
(defparameter *v-blank-mode* 1)
(defparameter *oam-mode* 2)
(defparameter *vram-mode* 3)

(defun lcd-mode-from-cycles (elapsed-cycles num-sprites)
  (let* ((elapsed-dots (* 4 elapsed-cycles))
	 (dot (mod elapsed-dots (lcd-dots)))
	 (mode0-dots (mode0-dots num-sprites))
	 (mode1-dots (mode1-dots))
	 (mode2-dots (mode2-dots))
	 (mode3-dots (mode3-dots num-sprites))
	 (scanline-dots (lcd-scanline-dots))
	 (scanline-dot (mod (- dot mode0-dots mode1-dots) scanline-dots)))
    (cond
      ((< dot mode0-dots) *h-blank-mode*)
      ((< dot (+ mode0-dots mode1-dots)) *v-blank-mode*)

      ((< scanline-dot mode2-dots) *oam-mode*)
      ((< scanline-dot (+ mode2-dots mode3-dots)) *vram-mode*)
      (t *h-blank-mode*))))

(defun pixels (width height)
  (make-array (* width height 4) :element-type '(unsigned-byte 8)))

(defparameter *lcd-width* 160)
(defparameter *lcd-height* 144)
(defvar *lcd-pixel-buffer* (pixels *lcd-width* *lcd-height*))
(defvar *lcd-oam-addresses* ())

;; Find the background tile-y, pixel-y

(defun scanline-background-pixel-y (scanline scroll-y)
  (mod (+ scanline scroll-y) (* 32 8)))

(defun scanline-background-tile-y (scanline scroll-y)
  (truncate (scanline-background-pixel-y scanline scroll-y) 8))

(defun scanline-background-tile-xs (scroll-x)
  (loop for tile-x from (truncate scroll-x 8)
	for i to (truncate 160 8)
	collecting (mod tile-x 32)))

(defun scanline-background-tile-indices (scanline scroll-x scroll-y)
  (let* ((tile-y (scanline-background-tile-y scanline scroll-y)))
    (mapcar (fn (+ % (* tile-y 32))) (scanline-background-tile-xs scroll-x))))

(defun scanline-background-tile-data-references (tile-map scanline scroll-x scroll-y)
  (mapcar (fn (aref tile-map %))
	  (scanline-background-tile-indices scanline scroll-x scroll-y)))

(defun tile-data-address (8000-addressing-mode? tile-data-reference)
  (if 8000-addressing-mode?
      (+ (* tile-data-reference 16) #x8000)
      (+ (* 16 (s8 tile-data-reference)) #x8800)))
(defun scanline-background-tile-data-addresses (8000-addressing-mode? tile-data-references)
  (mapcar
   (fn (tile-data-address 8000-addressing-mode? %))
   tile-data-references))


#+nil
(scanline-background-tile-data-references (background-tile-map) 64 0 0)
;; => (0 0 0 1 2 3 4 5 6 7 8 9 10 11 12 0 25 0 0 0 0)

#+nil
(scanline-background-tile-data-addresses
 (8000-addressing-mode?)
 (scanline-background-tile-data-references (background-tile-map) 64 0 0))

(defun tile-data (addr)
  (subseq *memory* addr (+ addr 16)))

(defun tile-data-color-byte-pair-y (tile-data tile-pixel-y)
  (subseq tile-data (* 2 tile-pixel-y) (* 2 (1+ tile-pixel-y))))

#+nil
(mapcar
 (fn (tile-data-color-byte-pair-y (tile-data %) 2))
 (scanline-background-tile-data-addresses
  (8000-addressing-mode?)
  (scanline-background-tile-data-references (background-tile-map) 64 0 0)))

(defun scanline-background-tile-color-byte-pairs (tile-map 8000-addressing-mode? scanline scroll-x scroll-y)
  (mapcar
   (fn (tile-data-color-byte-pair-y (tile-data %) (mod (+ scanline scroll-y) 8)))
   (scanline-background-tile-data-addresses
    8000-addressing-mode?
    (scanline-background-tile-data-references tile-map scanline scroll-x scroll-y))))

(defun color-byte-pair-palette-index (byte-pair)
  (tile-data-row-color-indices (aref byte-pair 1) (aref byte-pair 0)))


(defun scanline-background-pixels (scanline)
  (let* ((start-x (mod (scroll-x) 8)))
    (rgba-pixels-from-colors
     (subseq (mapcar
	      (fn (nth (palette-color-index (mem-byte *bg-color-palette-addr*) %) (default-palette-colors)))
	      (apply 'nconc (mapcar
			     (fn (color-byte-pair-palette-index %))
			     (scanline-background-tile-color-byte-pairs
			      (background-tile-map)
			      (8000-addressing-mode?)
			      scanline (scroll-x) (scroll-y)))))
	     start-x
	     (+ start-x 160)))))

(defun copy-scanline-to-lcd-pixel-buffer! (scanline-pixels scanline)
  (let* ((start (* scanline 160 4)))
    (loop for i from 0 below (length scanline-pixels) do
      (setf (aref *lcd-pixel-buffer* (+ i start))
	    (aref scanline-pixels i)))))

(defun update-lcd! (lcd-enable-start-cycle current-cycle)
  (let* ((current-mode (lcd-mode))
	 (num-sprites (length *lcd-oam-addresses*))
	 (elapsed-cycles (- current-cycle lcd-enable-start-cycle))
	 (mode (lcd-mode-from-cycles elapsed-cycles num-sprites)))
    (cond
      ((/= current-mode mode)
       (set-lcd-mode! mode)
       (cond
	 ((= mode *h-blank-mode*)
	  ;; start h-blank
	  ;; TODO: unlock OAM and VRAM	 
	  #+nil
	  (print (alist :h-blank elapsed-cycles))

	  ;; TODO: Optimize lcd-pixel-buffer-copy
	  (copy-scanline-to-lcd-pixel-buffer!
	   (scanline-background-pixels (lcd-scanline))
	   (lcd-scanline))

	  (when (lcds-h-blank-interrupt-enabled?)
	    (set-lcd-interrupt!)))
	 ((= mode *v-blank-mode*)
	  ;; start v-blank
	  #+nil
	  (print (alist :v-blank elapsed-cycles))

	  (when (interrupt-v-blank-enabled?)
	    (set-v-blank-interrupt!))
	  (when (lcds-v-blank-interrupt-enabled?)
	    (set-lcd-interrupt!)))
	 ((= mode *oam-mode*)
	  ;; Start OAM
	  #+nil
	  (print (alist :oam elapsed-cycles))
	  (set-lcd-scanline! (if (= current-mode 1)
				 0 ; reset if we just finished v-blank
				 ;; otherwise increment scanline.
				 (1+ (lcd-scanline))))
	  ;; TODO: lock OAM
	  ;; search OAM for first 10 sprites that overlap this scanline
	  ;; primary sort on x, secondary sort on address
	  ;; *lcd-oam-addresses*

	  (when (lcds-oam-interrupt-enabled?)
	    (set-lcd-interrupt!)))
	 ((= mode *vram-mode*)
	  #+nil
	  (print (alist :vram elapsed-cycles))
	  ;; start LCD display
	  ;; TODO: lock VRAM
	  )))
      ((= mode *v-blank-mode*)
       ;; When in v-blank mode, update the scanline
       (let* (;;(old-scanline (lcd-scanline))
	      )
	 (set-lcd-scanline! (lcd-scanline-from-cycles elapsed-cycles num-sprites))
	 #+nil
	 (when (/= old-scanline (lcd-scanline))
	   (print (alist :scanline (lcd-scanline)
			 :elapsed-cycles elapsed-cycles))))))))

;; Visualizations
;;; Sprites
;;;; Write position
;;;; show flipping

;; Add hardware events that were cycle-dependent
;;; events cause interrupts
;;; events switch modes in the LCD controller

;; TODO: Show when memory-registers have changed (using red/green)
;; TODO: Show what memory changed and in which region.

;; TODO: mark the instructions with
;;   conditional/unconditional jump
;;   return instr
(defun unconditional-jump-instr? (instr)
  (or (member (aval :name instr) '(:ret :reti :stop))
      (equal (aval :name instr) '(:jr :imm8))
      (equal (aval :name instr) '(:jp :imm16))
      (equal (aval :name instr) '(:jp :@hl))))
(defun return-instr? (instr)
  (member (aval :name instr) '(:ret :reti :stop :halt)))

(defun next-pcs (current-pc memory)
  (let* ((cpu (make-cpu :pc current-pc))
	 (next-instr (next-instr cpu memory)))
    (if next-instr
	(append
	 (unless (unconditional-jump-instr? next-instr)
	   (let* ((size (aval :instr-size next-instr)))
	     (if size
		 (list (+ current-pc size))
		 (progn
		   (warn "Instr ~A has no instr-size" next-instr)
		   nil))))
	 (when (and (aval :jump? next-instr) (not (return-instr? next-instr)))
	   (let* ((disassembly (disassemble-instr cpu memory)))
	     (list (aval :addr disassembly)))))
	(progn
	  (warn "Unknown instruction at ~A"  current-pc)
	  nil))))

(defun instr-addrs (memory start-pc)
  (nlet rec ((visited-pcs ())
	     (to-visit-pcs (list start-pc)))
    (if (null to-visit-pcs)
	(sort visited-pcs #'<)
	(let* ((pc (first to-visit-pcs)))
	  (rec (union visited-pcs (list pc))
	       (append (remove pc to-visit-pcs)
		       (remove-if (fn (or (member % visited-pcs) (>= % (length memory))))
				  (next-pcs pc memory))))))))

(defun disassemble-memory (memory &optional (start-pc 0))
  (mapcar (fn (acons :pc % (disassemble-instr (make-cpu :pc %) memory)))
	  (instr-addrs memory start-pc)))

(defun text-from-disassembly (disassembly)
  (format nil "~A ~A"
	  (hex16-text (aval :pc disassembly))
	  (let* ((name (aval :name disassembly)))
	    (if (listp name)
		(let* ((args (rest name))
		       (bindings
			 (mapcar (fn (cond
				       ((or (member % '(:@imm8 :@imm16))
					    (and (eql (first name) :jr)
						 (eql % :imm8)))
					(format nil "(~A)" (hex16-text (aval :addr disassembly %))))
				       (t (aval % disassembly %)))) args)))
		  (with-output-to-string (s)
		    (format s "~A " (first name))
		    (loop for binding in bindings
			  for i from 0 do
			    (format s "~A" binding)
			    (when (< i (1- (length bindings))) (format s " ")))))
		name))))

(defun disassemble-rom-into-text (memory &optional (start-pc 0))
  (with-output-to-string (s)
    (mapcar (fn (format s "~A~%" (text-from-disassembly %))) (disassemble-memory memory start-pc))))

(undefbreakpoint update-scroll-y (cpu memory)
		 (member (cpu-pc cpu) '(#x86)))

(defbreakpoint end-of-bios (cpu memory)
  (>= (cpu-pc cpu) #x100))

(defun memory-bank (rom bank)
  (subseq rom (* bank (* 16 1024)) (* (1+ bank) (* 16 1024))))

#+nil
(disassemble-instr (make-cpu :pc 1229) (subseq *cart-rom* 0 (* 16 1024)))
#+nil
(disassemble-rom-into-text (subseq *cart-rom* 0 (* 16 1024)) #x100)
#+nil
(remove-if-not
 (lambda (v)
   (let* ((name  (aval :name v)))
     (and (listp name)
	  (eq (first name) :ld)
	  (eq (second name) :@imm16)
	  (and (< (aval :addr v) #x8000)))))
 (disassemble-memory (memory-bank *cart-rom* 9) 0))

#+nil
(cart-type (cart-type-code *cart-rom*))
;; => :MBC1+RAM+BATTERY
;;(cart-rom-size (cart-rom-size-code *cart-rom*))
;; => ((512 :KB) 32)
;;(cart-ram-size (cart-ram-size-code *cart-rom*))
;; => ((8 :KB) (1 :BANKS))


;; Incorrect after startup?
;;    #xff48-#xff49: #x00 instead of #xff
;;     palette colors. irrelevant?

;; Move redraw of lcd display outside of execute!

#+nil
(disassemble-rom-into-text *bios-rom* 0)

;; TODO: pc-path should take into account which rom-bank we are in. (rom-bank . pc)

;; Add in more visualizations
;;  the last edited piece of memory
;;  A list of memory-writes in between continues
;;  add breakpoints that watch for changes in memory
;;  set color of lcd & interrupt enable registers based on changes to memory

#+nil
(with-output-to-string (s)
  (mapcar (fn
	    (format s "~A: ~A~%" (car %) (updated-memory-text (cdr %))))
	  (reverse (remove 7 *memory-updates* :key 'car))))

(defun register-visualization! (id visualization)
  (if *initialization-finished?*
      (funcall (aval :initialization visualization))
      (register-handler! (cons :initialization id)
			 (event-handler (event-matcher-initialization-finished)
					(aval :initialization visualization))))
  (when-aval (display-event-handler :display-event-handler visualization)
    (register-handler! (cons :display id) display-event-handler))
  (when-aval (hide-event-handler :hide-event-handler visualization)
    (register-handler! (cons :hide id) hide-event-handler))
  (let* ((update-event-handler (event-handler (event-matcher-update-visualization)
					      (aval :update visualization))))
    (register-handler! (cons :update id) update-event-handler)))

(defun remove-visuaulization! (id)
  (remove-handler! (cons :initialization id))
  (remove-handler! (cons :display id))
  (remove-handler! (cons :hide id))
  (remove-handler! (cons :update id)))

(defun visualization (&key initialization update display-event-handler hide-event-handler)
  (alist :type :visualization
	 :initialization initialization
	 :update update
	 :display-event-handler display-event-handler
	 :hide-event-handler hide-event-handler))

(defvisualization :main-lcd
    (visualization
     :initialization
     (fn
       (ensure-pixel-buffer-texture-loaded! :lcd 160 144)
       (loop for i from 0 below (length *lcd-pixel-buffer*)
	     do (setf (aref *lcd-pixel-buffer* i) 255))
       (replace-pixel-buffer! (gethash :lcd *textures*) *lcd-pixel-buffer*))
     :display-event-handler
     (display-event-handler
      :game
      (fn
	(setq *main-panel* :game)
	(add-drawing! :lcd (drawing 3 (fn (let* ((texture (gethash :lcd *textures*)))
					    (draw-texture! texture
							   0 0 (texture-width texture) (texture-height texture)
							   (g1 2) (g1 3) (* 160 3) (* 144 3))))))))
     :hide-event-handler
     (hide-event-handler :main-panel (fn (remove-drawing! :lcd)))
     :update
     (fn (replace-pixel-buffer! (gethash :lcd *textures*) *lcd-pixel-buffer*))))

(defvisualization :updated-memory
    (alist :initialization
	   (fn
	     (load-text-texture! :updated-memory :font "Memory: -")
	     (add-drawing! :updated-memory (drawing 1 (fn (draw-full-texture-id! :updated-memory (g2 32 15))))))
	   :update
	   (fn
	     (load-text-texture! :updated-memory :font
				 (updated-memory-text (aval :memory (instr-effects (cpu-current) *memory*)))))))


;; TODO: convert cpu-visualizations and memory visualizations to use defvisualization


;; TODO: a more compact way to specify & draw tables of values;
;;        |
;;        V
;; Label: Value
;; Label: Value
;; Label: Value
;; Label: Value

;; Table is a layout of labels & values positioned at top-center
;;  each value has a display type:
;;     flag [yes/no]
;;     addresses #x0000
;;     1-byte numbers
;;     2-byte numbers
;;  a value can be different colors
;;     white (no change)
;;     green/yellow (executing instr will change value)
;;     red/yellow (executing last instr changed value)


;; To specify the values
;;   how to get the value itself
;;   what display type the value has
;;   how to get the color

(defun cpu-data-color (cpu-fn previous-cpu-fn key)
  (let* ((cpu (funcall cpu-fn))
	 (previous-cpu (funcall previous-cpu-fn))
	 (will-change? (when cpu (aval key (instr-effects cpu *memory*))))
	 (changed? (when previous-cpu (aval key (instr-effects previous-cpu *memory*)))))
    (cond
      ((and changed? will-change?) (yellow))
      (changed? (red))
      (will-change? (green))
      (t (white)))))

(defun cpu-byte1-table-data (keys cpu-fn previous-cpu-fn)
  (mapcar (lambda (key)
	    (alist
	     :label (concat (symbol-name key) " ")
	     :value (fn (when-let (cpu (funcall cpu-fn))
			  (funcall (cpu-register-accessor-name key) cpu)))
	     :type :1-byte
	     :color (fn (cpu-data-color cpu-fn previous-cpu-fn key))))
	  keys))
(defun cpu-byte2-table-data (keys cpu-fn previous-cpu-fn)
  (mapcar (lambda (key)
	    (alist
	     :label (concat (symbol-name key) " ")
	     :value (fn (when-let (cpu  (funcall cpu-fn))
			  (funcall (cpu-register-accessor-name key) cpu)))
	     :type :2-byte
	     :color (fn (cpu-data-color cpu-fn previous-cpu-fn key))))
	  keys))

(defun draw-table-entry! (table-entry pos)
  (draw-full-texture-id-right-aligned! (aval :label-texture-id table-entry) pos)
  (let* ((color (funcall (aval :color table-entry)))
	 (value-texture-id (aval :value-texture-id table-entry)))
    (set-texture-color! value-texture-id color)
    (draw-full-texture-id! value-texture-id pos)))

(defun table-entry-value-text (table-entry)
  (let* ((type (aval :type table-entry))
	 (value (funcall (aval :value table-entry))))
    (ecase type
      (:1-byte (register8-text value))
      (:2-byte (register16-text value))
      (:address (hex16-text value))
      (:flag ""))))

(defun table-entry-value-texture-id (table-entry)
  (if (eql (aval :type table-entry) :flag)
      (if (funcall (aval :value table-entry)) :yes :no)
      (aval :value-texture-id table-entry)))

(defun update-table-entry-visualization! (table-entry)
  (let* ((type (aval :type table-entry))
	 (texture-id (table-entry-value-texture-id table-entry)))
    (unless (eql :flag type)
      (load-text-texture! texture-id :font (table-entry-value-text table-entry)))
    (aset :value-texture-id texture-id table-entry)))

(defun draw-table-data! (table-data pos)
  (mapcar (fn
	    (draw-table-entry! % pos)
	    (setq pos (v+ pos (g2 0 1))))
	  table-data))

(defun initialize-table-data-textures! (table-data)
  (mapcar (fn
	    (let* ((label (aval :label %))
		   (label-texture-id (gensym)))
	      (load-text-texture! label-texture-id :font label)
	      (amerge (alist :label-texture-id label-texture-id
			     :value-texture-id (if (eql (aval :type %) :flag)
						   :no
						   (gensym)))
		      %)))
	  table-data))

(defun table-visualization (table-data top-center-pos)
  (alist :initialization
	 (fn
	   (setq table-data (initialize-table-data-textures! table-data))
	   (add-drawing! (gensym) (drawing 3 (fn (draw-table-data! table-data top-center-pos)))))

	 :update
	 (fn (setq table-data (mapcar 'update-table-entry-visualization! table-data)))))

(defun merge-simple-visualizations (&rest visualizations)
  (alist :initialization (fn (mapcar (fn (when-aval (initialization :initialization %)
					   (funcall initialization)))
				     visualizations))
	 :update (fn (mapcar (fn (when-aval (update :update %)
				   (funcall update)))
			     visualizations))))

(defun cpu-table-visualization (cpu-fn previous-cpu-fn)
  (merge-simple-visualizations
   (table-visualization (cpu-byte1-table-data '(:a :b :d :h)
					      cpu-fn
					      previous-cpu-fn)
			(g2 4 4))
   (table-visualization (cpu-byte1-table-data '(:f :c :e :l)
					      cpu-fn
					      previous-cpu-fn)
			(g2 11 4))
   (table-visualization (cpu-byte2-table-data '(:af :bc :de :hl)
					      cpu-fn
					      previous-cpu-fn)
			(g2 18 4))))


(defun text-texture (text &optional (font-id :font))
  (alist :type :text-texture
	 :font-id font-id
	 :text text))

(defun drawings (&rest drawings)
  (alist :type :drawings
	 :drawings drawings))

(defun texture-drawing (texture-id pos &optional color)
  (alist :type :texture
	 :texture-id texture-id
	 :pos pos
	 :color color))

(defun right-aligned-drawing (drawing)
  (aset :right-aligned? t drawing))

(defun adjacent-textures-drawing (pos left-texture-id right-texture-id)
  (drawings
   (right-aligned-drawing (texture-drawing left-texture-id pos))
   (texture-drawing right-texture-id pos)))

(defun rows-drawing (fn top-pos &rest lists)
  (let* ((pos top-pos))
    (apply 'drawings
	   (apply 'mapcar
		  (lambda (&rest args)
		    (prog1 (apply fn pos args)
		      (setq pos (v+ pos (g2 0 1)))))
		  lists))))

(defun visualization-id (visualization id)
  (list :visualization (aval :id visualization) id))

(defun visualization-texture-ids (visualization)
  (let* ((static-texture-specs (aval :static-texture-specs visualization))
	 (dynamic-texture-spec-fns (aval :dynamic-texture-spec-fns visualization)))
    (append (akeys static-texture-specs)
	    (akeys dynamic-texture-spec-fns))))

(defun load-texture-spec! (id texture-spec)
  (ecase (aval :type texture-spec)
    (:text-texture (load-text-texture! id (aval :font-id texture-spec) (aval :text texture-spec)))))

(defmacro do-when-initialized (&body body)
  `(if *initialization-finished?*
       (progn ,@body)
       (register-one-off-handler! 'event-initialization-finished? (fn ,@body))))

(defun initialize-visualization! (visualization)
  (do-when-initialized
    (amap (fn (load-texture-spec! % %%))
	  (aval :static-texture-specs visualization))
    (update-visualization! visualization)
    (show-visualization! visualization)

    (register-handler! (visualization-id visualization :update)
		       (event-handler (fn (eql (aval :type %) :update-visualization))
				      (fn (update-visualization! visualization))))
    (amap 'register-handler! (aval :event-handlers visualization))))
(defun update-visualization! (visualization)
  (amap (fn (load-texture-spec! % (funcall %%)))
	(aval :dynamic-texture-spec-fns visualization)))
(defun show-visualization! (visualization)
  (amap (fn (add-drawing! % %%)) (aval :drawings visualization)))
(defun hide-visualization! (visualization)
  (amap (fn (remove-drawing! %)) (aval :drawings visualization)))
(defun unload-visualization! (visualization)
  (hide-visualization! visualization)
  (mapcar 'unload-texture! (visualization-texture-ids visualization))
  (remove-handler! (visualization-id visualization :update))
  (amap (fn (remove-handler! %)) (aval :event-handlers visualization)))

(defmacro with-visualization-ids (id (id-name &rest names) &body body)
  `(let* ,(cons
	   (list id-name id)
	   (mapcar (lambda (name) (list name `(list :visualization ,id ,(make-keyword name)))) names))
     ,@body))

(defvar *visualizations2* ())
(defun get-vis (id) (aval id *visualizations2*))
(defmacro defvis (id (id-name &rest names) &body visualization-alist)
  (let* ((vis-name (gensym)))
    `(let* ((,vis-name (with-visualization-ids ,id ,(cons id-name names)
			 (amerge (alist :id ,id)
				 ,@visualization-alist))))
       (asetq ,id ,vis-name *visualizations2*)
       (command! (initialize-visualization! ,vis-name))
       ,id)))
(defmacro undefvis (id &body ignored)
  (declare (ignore ignored))
  (let* ((vis-name (gensym)))
    `(let* ((,vis-name (aval ,id *visualizations2*)))
       (setq *visualizations2* (aremove *visualizations2* ,id))
       (command! (unload-visualization! ,vis-name))
       ,id)))

(defun merge-vis (&rest vis)
  (let* ((result ()))
    (loop for v in vis do
      (setq result (amap (fn (cons % (amerge (aval % result) %%))) v)))
    result))

(defvis :test-vis (id a b d h a-data b-data d-data h-data show hide drawing)
  (alist :static-texture-specs (alist a (text-texture "A ")
				      b (text-texture "B ")
				      d (text-texture "D ")
				      h (text-texture "H "))
	 ;; Get reloaded at update time
	 :dynamic-texture-spec-fns (alist a-data (fn (text-texture (register8-text (cpu-a (cpu-current)))))
					  b-data (fn (text-texture (register8-text (cpu-b (cpu-current)))))
					  d-data (fn (text-texture (register8-text (cpu-d (cpu-current)))))
					  h-data (fn (text-texture (register8-text (cpu-h (cpu-current))))))
	 :drawings (alist drawing (drawing 8 (rows-drawing
					      'adjacent-textures-drawing
					      (g2 4 8)
					      (list a b d h)
					      (list a-data b-data d-data h-data))))

	 :event-handlers (alist show (display-event-handler
				      :game
				      (fn (show-visualization! (get-vis id))))
				hide (hide-event-handler
				      :main-panel
				      (fn (hide-visualization! (get-vis id)))))))
