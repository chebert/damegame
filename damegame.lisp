;;;; damegame.lisp

(in-package #:damegame)

;; TODO:
;; GUI

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
(defparameter *width* 800 "The width of the window.")
(defparameter *height* 600 "The height of the window.")
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
	 (error "(CHECK ~S) failed" ,form))
       :check)))

(defmacro checkeql (a b)
  "Throws a helpful error if a is not EQL to b."
  (let ((a-val (gensym))
	(b-val (gensym)))
    `(let ((,a-val ,a)
	   (,b-val ,b))
       (unless (eql ,a-val ,b-val)
	 (error "~S => ~S does not EQL ~S => ~S" ',a ,a-val ',b ,b-val))
       :check)))

(defmacro fn (&body body)
  "Creates an anaphoric lambda with optional arguments % %% %%%."
  `(lambda (&optional % %% %%%)
     (declare (ignorable % %% %%%))
     ,@body))

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

(defstruct event-open-font
  id
  path
  size)
(defstruct event-font-opened
  font-id)

(defhandler handle-open-font! (event)
  (when (event-open-font-p event)
    (let* ((id  (event-open-font-id event))
	   (existing-font (gethash id *fonts*)))
      ;; If another font with the same id is already open, close it first.
      (when existing-font
	(close-font! existing-font))
      ;; Open the font and add it to the *fonts* hash-table
      (setf (gethash id *fonts*)
	    (open-font! (event-open-font-path event)
			(event-open-font-size event)))
      (notify-handlers! (make-event-font-opened :font-id id)))))

(defstruct event-texture-created
  texture-id)
(defstruct event-create-text-texture
  id
  font-id
  text)
(defhandler handle-create-text-texture! (event)
  (when (event-create-text-texture-p event)
    (let* ((id (event-create-text-texture-id event))
	   (font-id (event-create-text-texture-font-id event))
	   (font (gethash font-id *fonts*))
	   (existing-texture (gethash id *textures*)))
      (cond
	((null font)
	 ;; If the font doesn't exist, warn and don't do anyting else
	 (warn "Unable to find font ~S when creating text-texture ~S" font-id event))
	(t
	 ;; If another texture with the same id already exists, free it
	 (when existing-texture
	   (free-texture! existing-texture))
	 ;; Create the texture and add it to the textures hash-table
	 (setf (gethash id *textures*)
	       (create-text-texture! font (event-create-text-texture-text event)))
	 (notify-handlers! (make-event-texture-created :texture-id id)))))))

(defun draw-full-texture! (texture dx dy)
  (let* ((w (texture-width texture))
	 (h (texture-height texture)))
    (draw-texture! texture 0 0 w h dx dy w h)))

(defun v2 (x y) (vector x y))
(defun rect (x y w h) (vector x y w h))
(defun x (v) (aref v 0))
(defun y (v) (aref v 1))
(defun w (r) (aref r 2))
(defun h (r) (aref r 3))

(defun alist (&rest plist)
  (nlet rec ((plist plist)
	     (result ()))
    (if plist
	(let ((key (first plist))
	      (value (second plist))
	      (rest (rest (rest plist))))
	  (rec rest (acons key value result)))
	result)))
(defun aval (key alist)
  (cdr (assoc key alist)))
(defun aremove (alist &rest keys)
  (remove-if (fn (member % keys))
	     alist
	     :key 'car))
(defun aset (key value alist)
  (acons key value (aremove alist key)))
(defun akeys (alist)
  (mapcar 'car alist))
(defun amerge (old new)
  (nconc (apply 'aremove old (akeys new))
	 new))
(defun amap (fn alist)
  (mapcar (fn (funcall fn (car %) (cdr %)))
	  alist))


;; TODO: If we start adding to/removing from *DRAWINGS* frequently, then
;; something like a binary tree may be better
(defvar *drawings* ()
  "An association list of drawings.")

(defstruct drawing
  layer)

(defun sort-drawings-by-layer! ()
  "Sort *drawings* by layer."
  (setq *drawings* (sort *drawings* #'< :key (fn (drawing-layer (cdr %))))))
(defun remove-drawing! (drawing-id)
  "Removes the associated drawing from *drawings*"
  (setq *drawings* (aremove *drawings* drawing-id))
  (sort-drawings-by-layer!))

(defstruct (drawing-full-texture (:include drawing))
  texture-id
  pos)
(defmethod draw! (drawing-id (drawing drawing-full-texture))
  (let* ((pos (drawing-full-texture-pos drawing))
	 (texture-id (drawing-full-texture-texture-id drawing))
	 (texture (gethash texture-id *textures*)))
    (if texture
	(draw-full-texture! texture (x pos) (y pos))
	(progn
	  (warn "Unable to find texture ~S when drawing ~S. Removing drawing from *DRAWINGS*"
		texture-id
		drawing)
	  (aremove *drawings* drawing-id)))))

(defun color (r g b a) (vector r g b a))
(defun r (color) (aref color 0))
(defun g (color) (aref color 1))
(defun b (color) (aref color 2))
(defun a (color) (aref color 3))

(defstruct (drawing-fill-rect (:include drawing))
  color
  rect)
(defmethod draw! (drawing-id (drawing drawing-fill-rect))
  (let* ((color (drawing-fill-rect-color drawing))
	 (rect (drawing-fill-rect-rect drawing)))
    (set-draw-color! (r color) (g color) (b color) (a color))
    (fill-rect! (x rect) (y rect) (w rect) (h rect))))

(defun add-drawing! (id drawing)
  (setq *drawings* (aset id drawing *drawings*))
  (sort-drawings-by-layer!))

(defstruct event-add-drawing
  id
  drawing)

(defhandler handle-add-drawing! (event)
  (when (event-add-drawing-p event)
    (add-drawing! (event-add-drawing-id event) (event-add-drawing-drawing event))))


(defstruct event-remove-drawing id)
(defhandler handle-remove-drawing! (event)
  (when (event-remove-drawing-p event)
    (remove-drawing! (event-remove-drawing-id event))))

(defvar *quit?* nil
  "When true the main-loop! will terminate.")

(defvar *mouse-x* 0 "Mouse pixel x position measured from the left of the window.")
(defvar *mouse-y* 0 "Mouse pixel y position measured from the top of the window.")

(defvar *controls* ()
  "An association list of control-id to control of all of the currently active ccontrols.")
(defstruct control
  "A control is a rectangle which recieves (currently mouse-only) inputs."
  rect
  drawing-id
  hovered?
  pressed?
  click-fn)

(defstruct event-add-control
  id control)
(defhandler handle-add-control! (event)
  (when (event-add-control-p event)
    (let ((control (event-add-control-control event)))
      (setq *controls* (aset (event-add-control-id event)
			     control
			     *controls*))
      (add-drawing! (control-drawing-id control)
		    (make-drawing-fill-rect :layer 1 :color (color 0 0 0 255)
					    :rect (control-rect control))))))

(defun point-in-rect? (v2 r)
  (and (<= (x r) (x v2) (1- (+ (x r) (w r))))
       (<= (y r) (y v2) (1- (+ (y r) (h r))))))

(defun control-handle-mouse-move! (control)
  (let* ((rect (control-rect control))
	 (hovered? (control-hovered? control))
	 (pressed? (control-pressed? control))
	 (in-rect? (point-in-rect? (v2 *mouse-x* *mouse-y*) rect)))
    (cond
      ((and (not hovered?)
	    in-rect?)
       (setf (control-hovered? control) t)
       (add-drawing! (control-drawing-id control)
		     (make-drawing-fill-rect :layer 1 :color (color 255 0 255 255)
					     :rect rect)))
      ((and hovered?
	    (not in-rect?))
       (setf (control-hovered? control) nil)
       (when (not pressed?)
	 (add-drawing! (control-drawing-id control)
		       (make-drawing-fill-rect :layer 1 :color (color 0 0 0 255)
					       :rect rect)))))))

(defun control-handle-mouse-down! (control)
  (when (control-hovered? control)
    (setf (control-pressed? control) t)
    (add-drawing! (control-drawing-id control)
		  (make-drawing-fill-rect :layer 1 :color (color 255 0 0 255)
					  :rect (control-rect control)))))

(defvar *event-handlers* (make-hash-table))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *compiled-event-handlers* (make-hash-table)))

(defstruct event-control-clicked
  control-id)

(defun notify-handlers! (event)
  "Call the compiled & runtime event handlers with the given event."
  (maphash (fn (funcall %% event)) *compiled-event-handlers*)
  (maphash (fn (funcall %% event)) *event-handlers*))
(defun register-handler! (id fn)
  "Add/replace the handler in *event-handlers*"
  (setf (gethash id *event-handlers*) fn))
(defun remove-handler! (id)
  "Remove the handler from *event-handlers*"
  (remhash id *event-handlers*))
(defmacro defhandler (name (event) &body body)
  "Add/replace the handler NAME in *compiled-event-handlers*"
  `(setf (gethash ',name *compiled-event-handlers*)
	 (lambda (,event)
	   (declare (ignorable ,event))
	   ,@body)))
(defmacro undefhandler (name &rest rest)
  "Remove the handler NAME from *compiled-event-handlers*"
  (declare (ignore rest))
  `(remhash ',name *compiled-event-handlers*))

(defun control-handle-mouse-up! (control-id control)
  (when (and (control-pressed? control) (control-hovered? control))
    (notify-handlers! (make-event-control-clicked
		       :control-id control-id)))
  (setf (control-pressed? control) nil)
  (add-drawing! (control-drawing-id control)
		(make-drawing-fill-rect :layer 1 :color (color 0 0 0 255)
					:rect (control-rect control))))

(defun update! ()
  "Handles events, handles input events,
updates based on timestep, and renders to the screen."
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
	  (mapcar (fn (control-handle-mouse-move! (cdr %))) *controls*))
	 (event-mousedown
	  (when (eql :left (event-mousedown-button %))
	    (mapcar (fn (control-handle-mouse-down! (cdr %))) *controls*)))
	 (event-mouseup
	  (when (eql :left (event-mouseup-button %))
	    (mapcar (fn (control-handle-mouse-up! (car %) (cdr %))) *controls*))))))

  ;; Update based on time-step

  ;; Render to the screen
  (set-draw-color! 0 255 0 255)
  (clear!)
  (amap 'draw! *drawings*)
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
	 (setq *drawings* ())
	 (setq *events* ())
	 (setq *controls* ())
	 (start! *width* *height* *audio-frequency* *audio-channels*)
	 (main-loop!))
    (maphash (fn (close-font! %%)) *fonts*)
    (maphash (fn (free-texture! %%)) *textures*)
    (quit!)))


;; Last time (1/19):
;;  I made quit button by dumping global variables everywhere.
;;  I watched for mouse hover/unhover press/release events and set it to up to quit when it was clicked.
;;  I then abstracted out a table of "control"s, which can all be clicked, and replaced the global quit button
;;    with references to these controls.
;;  I quickly added a "click-fn" to the control, and moved the logic to quit the application into this.


;; This time (1/20):
;;  I want to replace the click-fn with a more robust notification system. CHECK.
;;    I want several observers to watch a single event. CHECK
;;    I want to be able to register/and unregister event-watchers. CHECK
;;    I want to be able to clear handlers that get added at runtime and still keep handlers that have been added at compile time. CHECK
;;  I want to chain events together. CHECK
;;    I want to be able to set up the example chain:
;;      open font -> create quit text texture -> create the quit text drawing && create the quit text control
;;      CHECK.


;; Some events for testing some drawings

(defun event-font-opened? (event font-id)
  (and (event-font-opened-p event)
       (eql font-id (event-font-opened-font-id event))))
(defun event-control-clicked? (event control-id)
  (and (event-control-clicked-p event)
       (eql control-id (event-control-clicked-control-id event))))
(defun event-texture-created? (event texture-id)
  (and (event-texture-created-p event)
       (eql texture-id (event-texture-created-texture-id event))))

#+nil
(event! (make-event-open-font :id :font :path "DroidSansMono.ttf" :size 16))

(defparameter *new-button-spec*
  (make-button-spec
   :control-id :new-button
   :font-id :font
   :texture-id (gensym)
   :text-drawing-id (gensym)
   :text "Wake up. Time to die."
   :bottom-layer 2
   :pos (v2 128 256)))
#+nil
(event! (make-event-create-button :button-spec *new-button-spec*))
#+nil
(event! (make-event-generic :fn (fn (destroy-button! *new-button-spec*))))


(defhandler handle-new-button-clicked (event)
  (when (event-control-clicked? event :new-button)
    (print "I'm so happy for you.")))

(defstruct event-create-button
  button-spec)
(defhandler handle-create-button! (event)
  (when (event-create-button-p event)
    (create-button! (event-create-button-button-spec event))))

(defstruct button-spec
  control-id
  font-id
  texture-id
  text-drawing-id
  text bottom-layer pos)
(defun create-button! (button-spec)
  (let ((font-id (button-spec-font-id button-spec))
	(texture-id (button-spec-texture-id button-spec))
	(control-id (button-spec-control-id button-spec))
	(text-drawing-id (button-spec-text-drawing-id button-spec))
	(control-drawing-id (gensym))
	(pos (button-spec-pos button-spec))
	(text (button-spec-text button-spec))
	(bottom-layer (button-spec-bottom-layer button-spec)))
    (let ((sym (gensym)))
      (register-handler!
       sym
       (lambda (event)
	 (when (event-texture-created? event texture-id)
	   (let* ((texture (gethash texture-id *textures*))
		  (rect (rect (x pos) (y pos) (texture-width texture) (texture-height texture))))
	     (notify-handlers!
	      (make-event-add-control :id control-id :control
				      (make-control :rect rect :hovered? nil :pressed? nil
						    :drawing-id control-drawing-id)))
	     (remove-handler! sym))))))

    (let ((font (gethash font-id *fonts*)))
      (if font
	  (progn
	    (notify-handlers! (make-event-create-text-texture :id texture-id :font-id font-id :text text))
	    (notify-handlers! (make-event-add-drawing :id text-drawing-id
						      :drawing (make-drawing-full-texture
								:layer (1+ bottom-layer)
								:texture-id texture-id :pos pos))))
	  (let ((sym (gensym)))
	    (register-handler!
	     sym
	     (lambda(event)
	       (when (event-font-opened? event font-id)
		 (notify-handlers! (make-event-create-text-texture :id texture-id :font-id font-id :text text))
		 (notify-handlers! (make-event-add-drawing :id text-drawing-id
							   :drawing (make-drawing-full-texture
								     :layer (1+ bottom-layer)
								     :texture-id texture-id :pos pos)))
		 (remove-handler! sym)))))))))

(defstruct event-destroy-text-texture
  id)
(defhandler handle-destroy-text-texture! (event)
  (when (event-destroy-text-texture-p event)
    (let* ((id (event-destroy-text-texture-id event))
	   (texture (gethash id *textures*)))
      (when texture
	(free-texture! texture)
	(remhash id *textures*)))))
(defstruct event-remove-control
  id)
(defhandler handle-remove-control! (event)
  (when (event-remove-control-p event)
    (let* ((id (event-remove-control-id event))
	   (control (aval id *controls*)))
      (when control
	(notify-handlers! (make-event-remove-drawing :id (control-drawing-id control)))
	(setq *controls* (aremove *controls* id))))))

(defstruct event-generic
  fn)
(defhandler handle-generic-event! (event)
  (when (event-generic-p event)
    (funcall (event-generic-fn event))))

(defun destroy-button! (button-spec)
  (let ((texture-id (button-spec-texture-id button-spec))
	(control-id (button-spec-control-id button-spec))
	(text-drawing-id (button-spec-text-drawing-id button-spec)))
    (notify-handlers! (make-event-remove-drawing :id text-drawing-id))
    (notify-handlers! (make-event-destroy-text-texture :id texture-id))
    (notify-handlers! (make-event-remove-control :id control-id))))

(let ((font-id :font)
      (texture-id :quit-text)
      (text-drawing-id :quit-text-drawing)
      (control-id :quit-control)
      (control-drawing-id :quit-control-drawing)
      (text "Quit")
      (bottom-layer 1)
      (pos (v2 120 200)))

  (defhandler font-opened (event)
    (when (event-font-opened? event font-id)
      (notify-handlers! (make-event-create-text-texture :id texture-id :font-id font-id :text text))
      (notify-handlers! (make-event-add-drawing :id text-drawing-id
						:drawing (make-drawing-full-texture
							  :layer (1+ bottom-layer)
							  :texture-id texture-id :pos pos)))))
  
  (defhandler quit-button-clicked (event)
    (when (event-control-clicked? event control-id)
      (setq *quit?* t)))

  (defhandler quit-texture-created (event)
    (when (event-texture-created? event texture-id)
      (let* ((texture (gethash texture-id *textures*))
	     (rect (rect (x pos) (y pos) (texture-width texture) (texture-height texture))))
	(notify-handlers!
	 (make-event-add-control :id control-id :control
				 (make-control :rect rect :hovered? nil :pressed? nil
					       :drawing-id control-drawing-id)))))))


#+nil
(mapcar
 'event!
 (list
  (make-event-create-text-texture :id :text :font-id :font :text "Hello, cruel world")
  (make-event-add-drawing
   :id :drawing
   :drawing (make-drawing-full-texture :layer 2 :texture-id :text :pos (v2 40 40)))))


#+nil
(event!
 (make-event-add-drawing
  :id :background
  :drawing (make-drawing-fill-rect :layer 1 :color (color 0 0 0 0)
				   :rect (let ((texture (gethash :text *textures*)))
					   (rect 40 40
						 (texture-width texture)
						 (texture-height texture))))))

#+nil
(event!
 (make-event-remove-drawing :id :background))
