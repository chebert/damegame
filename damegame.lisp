;;;; damegame.lisp

(in-package #:damegame)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-keyword (symbol)
    (intern (symbol-name symbol) (find-package :keyword)))
  (defun symbolicate (&rest symbols)
    (intern (apply 'concatenate 'string (mapcar 'symbol-name symbols))))
  (defvar *tests* (make-hash-table))
  
  (defvar *commands* ()))

(defmacro command! (&body body)
  `(if *quit?*
       (progn ,@body)
       (push (fn ,@body) *commands*)))

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
(defvar *textures* (make-hash-table))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
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
  (defun akeep (alist &rest keys)
    (remove-if-not (fn (member % keys))
		   alist
		   :key 'car))
  (defun aset (key value alist)
    "Return a new alist with the value associated with key added or replaced."
    (acons key value (aremove alist key)))
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
    `(setq ,alist-name (aset ,id ,value ,alist-name))))

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

(defvar *event-handlers* (make-hash-table)
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

(defun event-initialization-finished ()
  (alist :type :initialization-finished))

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
	 (start! *width* *height* *audio-frequency* *audio-channels*)
	 (load-font! :font "DroidSansMono.ttf" 16)
	 (event! (event-initialization-finished))
	 (main-loop!))
    (maphash (fn (close-font! %%)) *fonts*)
    (maphash (fn (free-texture! %%)) *textures*)
    (clrhash *fonts*)
    (clrhash *textures*)
    (clrhash *event-handlers*)
    (quit!)))

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

;; Creation: text, layer, pos, font-id,
(defun button (text layer pos font-id)
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
	 (drawing-id (aval :drawing-id button)))
    (unload-texture! texture-id)
    (remove-drawing! drawing-id)
    (setq *buttons* (aremove *buttons* button-id))))

(defun button-color (button)
  (cond
    ((aval :pressed? button) (red))
    ((aval :hovered? button) (grey 128))
    (t (grey 60))))

(defhandler handle-intialize-buttons (event)
	    (event-matcher-font-opened :font)
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

(defvar *cpus* (list (cpu-initial)))
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
					 (funcall byte-text-fn (aref memory (+ i start-addr))))))))

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

(defun initialize-memory-visualization! (id memory-visualization)
  (let* ((drawing-id (gensym)))
    (set-memory-visualization! id (aset :drawing-id drawing-id memory-visualization))
    (update-memory-visualization! *memory* memory-visualization)
    (add-drawing! drawing-id (drawing 1 (fn (draw-memory-visualization! (cpu-current) id))))
    (load-text-texture! (aval :title-texture-id memory-visualization) :font
			(aval :title memory-visualization))))

(defmemory-visualization pc (memory-visualization "PC" (g2 1 3) (fn (start-addr (cpu-pc (cpu-current))))
						  'register8-text))
(defmemory-visualization stack (memory-visualization "Stack" (g2 11 3) (fn (start-addr (cpu-sp (cpu-current))))
						     'register8-text))
(defmemory-visualization hl (memory-visualization "HL" (g2 21 3) (fn (start-addr (cpu-hl (cpu-current))))
						  'register8-text))

(defhandler handle-initialize-memory-visualization (event)
	    (event-matcher-font-opened :font)
  (amap 'initialize-memory-visualization! *memory-visualizations*))


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
      (let ((column (+ (g1 11) (x cpu-pos))))
	(incf y (g1 1))
	(draw-full-texture-id-right-aligned! :stack-pointer (v2 column y))
	(set-texture-color! (aval :sp cpu-visualization) (aval :sp cpu-colors (white)))
	(draw-full-texture-id! (aval :sp cpu-visualization) (v2 column y))

	(incf y (g1 1))
	(draw-full-texture-id-right-aligned! :program-counter (v2 column y))
	(set-texture-color! (aval :pc cpu-visualization) (aval :pc cpu-colors (white)))
	(draw-full-texture-id! (aval :pc cpu-visualization) (v2 column y))))))

(defun initialize-cpu-visualization! (id vis)
  (let ((drawing-id (gensym)))
    (set-cpu-visualization! id (aset :drawing-id drawing-id vis))
    (load-text-texture! (aval :title-texture-id vis) :font (aval :title vis))
    (update-cpu-visualization! vis)
    (add-drawing! drawing-id (drawing *cpu-state-layer* (fn (draw-cpu-visualization! id))))))

(defcpu-visualization current
    (cpu-visualization "Current" (g2 32 18)
		       'cpu-current
		       (fn (or (cpu-previous) (cpu-initial)))))
(defcpu-visualization previous
    (cpu-visualization "Previous" (g2 32 2)
		       (fn (or (cpu-previous) (cpu-initial)))
		       (fn (or (third *cpus*) (cpu-initial)))))

(defhandler load-cpu-visualization (event)
	    (event-matcher-font-opened :font)
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

  (amap 'initialize-cpu-visualization! *cpu-visualizations*))

(defun flag-state-texture-id (set?)
  (if set? :yes :no))

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
(defun hex16-text (num)
  (format nil "0x~4,'0x" num))
(defun register8-text (register)
  (ecase *number-base*
    (:hexadecimal
     (hex8-text register))
    (:signed
     (format nil "~d" (s8 register)))
    (:unsigned
     (format nil "~d" register))
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

(defun long-instr? (byte1)
  (eql byte1 #xcb))

(defun next-instr (cpu memory)
  (let* ((pc (cpu-pc cpu))
	 (byte (aref memory pc)))
    (if (long-instr? byte)
	(let* ((byte2 (aref memory (1+ pc))))
	  (aval byte2 *long-instrs*))
	(aval byte *instrs*))))

(defun disassemble-instr (cpu memory)
  (let* ((pc (cpu-pc cpu))
	 (byte (aref memory pc)))
    (if (long-instr? byte)
	(let* ((byte2 (aref memory (1+ pc)))
	       (instr (aval byte2 *long-instrs*)))
	  (if instr
	      (list* (aval :name instr) (funcall (aval :disassemble-instr instr) cpu memory))
	      (list :unknown (register16-text (combined-register byte byte2)))))
	(let* ((instr (aval byte *instrs*)))
	  (if instr
	      (list* (aval :name instr) (funcall (aval :disassemble-instr instr) cpu memory))
	      (list :unknown (hex8-text byte)))))))

(run-tests!)

(defvar *instruction-description-ids* (loop for i below 8 collecting (gensym)))

(defun reset! ()
  (setq *cpus* (list (cpu-initial)))
  (reset-memory!)
  (read-rom-file-into-memory! "gb_bios.bin")

  (update-visualizations! nil))

(defun handle-execute-button-clicked! ()
  (setq *cpus* (list (cpu-current) (copy-cpu (cpu-current)) (cpu-previous)))
  (execute! (cpu-current) *memory*)
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

(defhandler handle-initialize-cpu-visualization (event)
	    (event-matcher-font-opened :font)
  (load-text-texture! :disassembly :font "(Disassembly)")
  (add-drawing! :disassembly (full-texture-drawing 1 :disassembly (g2 32 13)))

  (initialize-description! (aval :description (next-instr (cpu-current) *memory*)))
  
  (load-text-texture! :disassembly-next :font (disassembly-text (cpu-current) *memory*))
  (add-drawing! :disassembly-next (full-texture-drawing 1 :disassembly-next (g2 32 29))))

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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *register8-keys* '(:a :b :c :d :e :f :h :l)))
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

(defun update-visualizations! (&optional (cpu-previous (cpu-previous)))
  (let* ((prev-disassembly-text (if cpu-previous
				    (disassembly-text cpu-previous *memory*)
				    "(Disassembly)")))
    
    (amap (fn (update-memory-visualization! *memory* %%)) *memory-visualizations*)
    (amap (fn (update-cpu-visualization! %%)) *cpu-visualizations*)
    
    (load-text-texture! :disassembly :font (if prev-disassembly-text
					       prev-disassembly-text
					       "(Disassembly)"))
    (load-text-texture! :disassembly-next :font (disassembly-text (cpu-current) *memory*))
    (initialize-description! (aval :description (next-instr (cpu-current) *memory*)))))

(defbutton execute (button "Execute!" 1 (g2 32 30) :font)
  (handle-execute-button-clicked!))

(defbutton reset (button "Reset" 1  (g2 32 31) :font)
  (reset!))

(defbutton refresh (button "Refresh" 1 (g2 32 32) :font)
  (update-visualizations!))

(defbutton continue (button "Continue!" 1 (g2 37 30) :font)
  (loop until (break? (cpu-current) *memory*)
	do (setq *cpus* (list (cpu-current) (copy-cpu (cpu-current)) (cpu-previous)))
	   (execute! (cpu-current) *memory*))
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
	    (event-matcher-font-opened :font)
  (load-text-texture! :mouse-pos :font (mouse-pos-text))
  (add-mouse-pos-drawing!))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *instrs* ())
  (defvar *long-instrs* ())

  (defun compile-instr-effects (instr-cpu-name instr-memory-name bindings effects)
    `(lambda (,instr-cpu-name ,instr-memory-name)
       (declare (ignorable ,instr-cpu-name ,instr-memory-name))
       (let* ,bindings
	 (alist ,@(apply 'nconc (amap (fn (list % %%)) (aremove effects :memory)))
		,@(let ((memory (aval :memory effects)))
		    (list :memory (cons 'list (mapcar (fn (cons 'list %)) memory))))))))

  (defun compile-disassemble-instr (instr-cpu-name instr-memory-name bindings disassembly)
    (let ((cpu-name (gensym))
	  (memory-name (gensym)))
      `(lambda (,cpu-name ,memory-name)
	 (let* ((,instr-cpu-name ,cpu-name)
		(,instr-memory-name ,memory-name))
	   (declare (ignorable ,instr-cpu-name ,instr-memory-name))
	   (let* ,bindings
	     (declare (ignorable ,@(mapcar 'first bindings) ))
	     ,disassembly)))))

  (defun compile-definstr (whole byte cpu-name memory-name bindings cycles disassembly pc alist-name)
    (unless pc
      (warn "PC should be provided"))
    (unless cycles
      (warn "CYCLES should be provided"))
    (let* ((properties (apply 'alist (rest (rest (rest (rest whole))))))
	   (effects (aremove properties :name :description :disassembly)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (asetq ',byte (alist :cpu-name ',cpu-name
			      :memory-name ',memory-name
			      :bindings ',bindings
			      :byte ',byte
			      :instr-effects ,(compile-instr-effects cpu-name memory-name bindings effects)
			      :disassemble-instr ,(compile-disassemble-instr cpu-name memory-name bindings disassembly)
			      ,@ (apply 'nconc (amap (fn `(,% ',%%)) properties)))
		,alist-name))))

  (defun compile-execute ()
    (let ((cpu-name (gensym "CPU"))
	  (memory-name (gensym "MEMORY"))
	  (pc-name (gensym "PC")))
      `(defun execute! (,cpu-name ,memory-name)
	 (let ((,pc-name (cpu-pc ,cpu-name)))
	   (ecase (aref ,memory-name ,pc-name)
	     ,@(amap (fn (list % (compile-instr-spec-for-execute cpu-name memory-name %%))) *instrs*)
	     (#xcb
	      ;; TODO: optimize pc-access
	      (ecase (aref ,memory-name (1+ ,pc-name))
		,@(amap (fn (list % (compile-instr-spec-for-execute cpu-name memory-name %%))) *long-instrs*))))))))

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
      (:hl 'cpu-hl)))

  (defun compile-register-set (key instr-spec)
    (let ((value (aval key instr-spec)))
      (when value `((,(cpu-register-accessor-name key) ,(aval :cpu-name instr-spec)) ,value))))

  (defun compile-register-sets (instr-spec)
    (apply 'nconc
	   (mapcar (fn (compile-register-set % instr-spec))
		   '(:a :b :c :d :e :f :h :l :sp :pc))))

  (defun compile-combined-register-set (instr-spec combined-key lo-key hi-key)
    (let ((combined-value (aval combined-key instr-spec))
	  (cpu-name (aval :cpu-name instr-spec))
	  (combined-name (gensym)))
      (when combined-value
	`(let ((,combined-name ,combined-value))
	   (setf (,(cpu-register-accessor-name lo-key) ,cpu-name) (lo-byte ,combined-name)
		 (,(cpu-register-accessor-name hi-key) ,cpu-name) (hi-byte ,combined-name))))))

  (defun compile-combined-register-sets (instr-spec)
    (remove 'nil (mapcar (fn (apply 'compile-combined-register-set instr-spec %))
			 '((:af :f :a)
			   (:bc :c :b)
			   (:de :e :d)
			   (:hl :l :h)))))
  (defun compile-set-flag (cpu-flag-name flag-key bit-index instr-spec)
    (if (akey? flag-key instr-spec)
	`(if ,(aval flag-key instr-spec) 1 0)
	`(bit-value ,cpu-flag-name ,bit-index)))
  
  (defun compile-set-flags (instr-spec)
    (when (some (fn (akey? % instr-spec)) '(:zero? :subtraction? :half-carry? :carry?))
      (let ((cpu-flag-name (gensym))
	    (cpu-name (aval :cpu-name instr-spec)))
	`((cpu-flag ,(aval :cpu-name instr-spec))
	  (let ((,cpu-flag-name (cpu-flag ,cpu-name)))
	    (declare (ignorable ,cpu-flag-name))
	    (flag ,(compile-set-flag cpu-flag-name :zero? 3 instr-spec)
		  ,(compile-set-flag cpu-flag-name :subtraction? 2 instr-spec)
		  ,(compile-set-flag cpu-flag-name :half-carry? 1 instr-spec)
		  ,(compile-set-flag cpu-flag-name :carry? 0 instr-spec)))))))

  (defun compile-set-memory (instr-spec)
    (let ((memory (aval :memory instr-spec)))
      (apply 'concatenate 'list (mapcar (fn `((aref ,(aval :memory-name instr-spec) ,(first %)) ,(second %))) memory))))

  (defun compile-instr-spec-for-execute (cpu-name memory-name instr-spec)
    `(let* ((,(aval :cpu-name instr-spec) ,cpu-name)
	    (,(aval :memory-name instr-spec) ,memory-name))
       (declare (ignorable ,(aval :cpu-name instr-spec)
			   ,(aval :memory-name instr-spec)))
       (let* ,(aval :bindings instr-spec)
	 ,@(compile-combined-register-sets instr-spec)
	 (setf
	  ,@(compile-set-memory instr-spec)
	  ,@(compile-set-flags instr-spec)
	  ,@(compile-register-sets instr-spec))))))

(defmacro definstr (&whole whole byte (cpu-name memory-name)
		      bindings &key
				 name description
				 disassembly
				 ;; effects
				 jump?
				 zero? subtraction? half-carry? carry?
				 a b c d e f h l
				 af bc de hl
				 pc sp
				 cycles
				 memory)
  (declare (ignore name description jump?
		   zero? subtraction? half-carry? carry?
		   a b c d e f h l
		   af bc de hl
		   sp
		   memory))
  (compile-definstr whole byte cpu-name memory-name bindings cycles disassembly pc '*instrs*))

(defmacro deflong-instr (&whole whole byte (cpu-name memory-name)
			   bindings &key
				      name description
				      disassembly
				      ;; Effects
				      jump?
				      zero? subtraction? half-carry? carry?
				      a b c d e f h l
				      af bc de hl
				      pc sp
				      cycles
				      memory)
  (declare (ignore name description
		   jump?
		   zero? subtraction? half-carry? carry?
		   a b c d e f h l
		   af bc de hl
		   sp
		   memory))
  (compile-definstr whole byte cpu-name memory-name bindings cycles disassembly pc '*long-instrs*))

(defmacro undefinstr (byte &body ignored)
  (declare (ignore ignored))
  `(progn
     (setq *instrs* (aremove *instrs* ,byte))))
(defmacro undeflong-instr (byte &body ignored)
  (declare (ignore ignored))
  `(progn
     (setq *long-instrs* (aremove *long-instrs* ,byte))))


(eval-when (:compile-toplevel :load-toplevel :execute)
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


  (defun opcode-from-template (opcode-template opcode-parameter-bindings opcode-arguments)
    (let* ((opcode-parameters (mapcar (fn (cons (third %) %%))
				      opcode-parameter-bindings
				      opcode-arguments)))
      (apply 'logior
	     opcode-template
	     (amap (fn (ash %% %)) opcode-parameters))))

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
      (#b101 :l))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compile-definstr-class (deflong-instr-or-definstr opcode-template opcode-parameter-bindings cpu memory instr-spec)
    (let* ((opcode-parameter-lists (mapcar (fn (eval (second %))) opcode-parameter-bindings)))
      `(progn
	 ,@(apply
	    'map-cartesian
	    (lambda (opcode-arguments)
	      `(,deflong-instr-or-definstr
		   ,(opcode-from-template opcode-template opcode-parameter-bindings opcode-arguments) (,cpu ,memory)
		 ;; TODO: can we get rid of this eval?
		 ,@(eval `(let* ,(opcode-bindings opcode-parameter-bindings opcode-arguments)
			    ,@instr-spec))))
	    opcode-parameter-lists)))))

(defmacro definstr-class ((opcode-template &rest opcode-parameter-bindings) (cpu memory) &body instr-spec)
  (compile-definstr-class 'definstr opcode-template opcode-parameter-bindings cpu memory
			  instr-spec))

(defmacro deflong-instr-class ((opcode-template &rest opcode-parameter-bindings) (cpu memory) &body instr-spec)
  (compile-definstr-class 'deflong-instr opcode-template opcode-parameter-bindings cpu memory
			  instr-spec))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun opcode-from-template2 (opcode-template opcode-argument-indices opcode-arguments)
    (let* ((opcode-parameters (mapcar (fn (cons % %%))
				      opcode-argument-indices
				      opcode-arguments)))
      (apply 'logior
	     opcode-template
	     (amap (fn (ash %% %)) opcode-parameters))))

  (defun map-opcodes (fn opcode-template opcode-list-parameters)
    (let ((opcode-parameter-lists (mapcar 'cdr opcode-list-parameters))
	  (opcode-parameter-indices (mapcar 'car opcode-list-parameters)))
      (apply 'map-cartesian
	     (lambda (arguments)
	       (let* ((opcode (opcode-from-template2 opcode-template opcode-parameter-indices arguments)))
		 (apply fn opcode arguments)))
	     opcode-parameter-lists)))

  (defun merge-instr-specs (old &rest new-instr-specs)
    (if new-instr-specs
	(let* ((new (first new-instr-specs)))
	  (let* ((old-bindings (aval :bindings old))
		 (new-bindings (aval :bindings new))
		 (bindings (append old-bindings new-bindings)))
	    (apply 'merge-instr-specs
		   (aset :bindings bindings (amerge old new))
		   (rest new-instr-specs))))
	old)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *instr-specs* ()))

(defun compile-instr-spec-effects (instr-spec)
  (compile-instr-effects (aval :cpu-name instr-spec)
			 (aval :memory-name instr-spec)
			 (aval :bindings instr-spec)
			 (akeep instr-spec
				:a :b :c :d :e :f :h :l
				:af :bc :de :hl
				:sp :pc
				:memory
				:cycles
				:zero? :carry? :half-carry? :subtraction?)))
(defun compile-instr-spec-disassemble-instr (instr-spec)
  (compile-disassemble-instr (aval :cpu-name instr-spec)
			     (aval :memory-name instr-spec)
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

(defun compile-key-accessor (key &key (cpu-name 'cpu) (memory-name 'memory) (imm8-name 'imm8) (imm16-name 'imm16))
  (cond
    ((member key *register-keys*) `(,(cpu-register-accessor-name key) ,cpu-name))
    ((eql key :imm8) imm8-name)
    ((eql key :@imm8) `(aref ,memory-name (+ #xff00 ,imm8-name)))
    ((eql key :@imm16) `(aref ,memory-name ,imm16-name))
    ((eql key :@bc) `(aref ,memory-name (cpu-bc ,cpu-name)))
    ((eql key :@de) `(aref ,memory-name (cpu-de ,cpu-name)))
    ((eql key :@af) `(aref ,memory-name (cpu-af ,cpu-name)))
    ((member key '(:@hl :@hli :@hld)) `(aref ,memory-name (cpu-hl ,cpu-name)))
    ((eql key :@c) `(aref ,memory-name (+ #xff00 (cpu-c ,cpu-name))))
    ((eql key :imm16) imm16-name)
    (t (error "unimplemented ~A" key))))
(defun key-set-instr-spec (key value-form &key (cpu-name 'cpu) (imm8-name 'imm8) (imm16-name 'imm16))
  (let ((@hl-form `(((cpu-hl ,cpu-name) ,value-form))))
    (cond
      ((member key *register-keys*) (alist key value-form))
      ((eql key :@imm8) (alist :memory `(((+ #xff00 ,imm8-name) ,value-form))))
      ((eql key :@imm16) (alist :memory `((,imm16-name (lo-byte ,value-form))
					  ((1+ ,imm16-name) (hi-byte ,value-form)))))
      ((eql key :@bc) (alist :memory `(((cpu-bc ,cpu-name) ,value-form))))
      ((eql key :@de) (alist :memory `(((cpu-de ,cpu-name) ,value-form))))
      ((eql key :@af) (alist :memory `(((cpu-af ,cpu-name) ,value-form))))
      ((eql key :@hl) (alist :memory @hl-form))
      ((eql key :@c) (alist :memory `(((+ #xff00 (cpu-c ,cpu-name)) ,value-form))))
      ((eql key :@hli) (alist :memory @hl-form
			      :hl `(1+ (cpu-hl ,cpu-name))))
      ((eql key :@hld) (alist :memory @hl-form
			      :hl `(1- (cpu-hl ,cpu-name))))
      (t (error "unimplemented ~A" key)))))

(defun imm8-instr-spec (&key (pc-name 'pc) (memory-name 'memory) (imm8-name 'imm8))
  (alist
   :bindings `((,imm8-name (aref ,memory-name (1+ ,pc-name))))
   :disassembly `(alist :imm8 (register8-text ,imm8-name))))
(defun imm16-instr-spec (&key (pc-name 'pc) (memory-name 'memory) (imm16-name 'imm16))
  (alist
   :bindings `((,imm16-name (combined-register (aref ,memory-name (+ 2 ,pc-name)) (aref ,memory-name (1+ ,pc-name)))))
   :disassembly `(alist :imm16 (register16-text ,imm16-name))))

(defun instr-spec-defaults (&key (cpu-name 'cpu) (memory-name 'memory) (pc-name 'pc) opcode cycles)
  (amerge
   (alist
    :cpu-name cpu-name
    :memory-name memory-name
    :bindings `((,pc-name (cpu-pc ,cpu-name))))
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

(defun ld-instr-spec (opcode contents-key into-key &key 
						     (pc-name 'pc)
						     (cpu-name 'cpu)
						     (imm8-name 'imm8)
						     (imm16-name 'imm16)
						     (memory-name 'memory))
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
     (instr-spec-defaults :cpu-name cpu-name
			  :memory-name memory-name
			  :pc-name pc-name
			  :opcode opcode
			  :cycles cycles)
     (when imm8?
       (imm8-instr-spec :pc-name pc-name :memory-name memory-name :imm8-name imm8-name))
     (when imm16?
       (imm16-instr-spec :pc-name pc-name :memory-name memory-name :imm16-name imm16-name))
     (when (member contents-key '(:@hli))
       (alist :hl `(1+ (cpu-hl ,cpu-name))))
     (when (member contents-key '(:@hld))
       (alist :hl `(1- (cpu-hl ,cpu-name))))
     (alist
      :name (ld-name contents-key into-key)
      :description (ld-description contents-key into-key)
      :pc `(+ ,instr-size ,pc-name))
     (key-set-instr-spec into-key (compile-key-accessor contents-key)
			 :cpu-name cpu-name :imm8-name imm8-name))))


(defun ld-8bit-class-instr-specs (opcode opcode-list-parameters fn)
  (class-instr-specs
   opcode opcode-list-parameters
   (lambda (opcode &rest register-codes)
     (apply fn opcode (mapcar 'register-key register-codes)))))

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
    (ld-instr-spec #b11110000 :@imm8 :a)
    (ld-instr-spec #b11100000 :a :@imm8)
    
    (ld-instr-spec #b11111010 :@imm16 :a)
    (ld-instr-spec #b11101010 :a :@imm16)

    ;; TODO: create HL binding to optimize
    (ld-instr-spec #b00101010 :@hli :a)
    (ld-instr-spec #b00111010 :@hld :a)

    (ld-instr-spec #b00000010 :a :@bc)
    (ld-instr-spec #b00010010 :a :@de)

    (ld-instr-spec #b00100010 :a :@hli)
    (ld-instr-spec #b00110010 :a :@hld))))

(defun imm16 (addr memory)
  (combined-register (aref memory (1+ addr)) (aref memory addr)))

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
	 (alist :opcode opcode
		:name (list :push combined-key)
		:description (format nil "Push the contents of register pair ~A onto the stack." combined-key)
		:pc '(1+ pc)
		:cycles 4))))
    #b11000101 (alist 4 *register-pair-codes*))

   ;; POP qq
   (map-opcodes
    (lambda (opcode register-pair)
      (let* ((register-pair-key (qq-register-pair-key register-pair)))
	(merge-instr-specs
	 (instr-spec-defaults)
	 (pop-instr-spec register-pair-key)
	 (alist :opcode opcode
		:name (list :pop register-pair-key)
		:description (format nil "Pop the 16-bit value off the stack into register pair ~A." register-pair-key)
		:pc '(1+ pc)
		:cycles 3))))
    #b11000001 (alist 4 *register-pair-codes*))

   (list
    ;; LDHL SP, s8
    (merge-instr-specs
     (instr-spec-defaults)
     (alist :opcode #b11111000
	    :name (list :ld :hl :sp+s8)
	    :description "Add signed imm8 value to sp and store the
result in HL."
	    :bindings '((s8 (s8 (aref memory (1+ pc))))
			(sp (cpu-sp cpu)))
	    :disassembly '(alist :s8 (register8-text s8))
	    :hl '(+ s8 sp)
	    :pc '(+ 2 pc)
	    :cycles 3

	    ;; 16-bit addition flags?
	    :zero? nil
	    :subtraction? nil
	    :half-carry? '(bit-carry? 11 sp s8)
	    :carry? '(bit-carry? 15 sp s8)))
    
    (ld-instr-spec #b00001000 :sp :@imm16))))

(defun push-instr-spec (register-pair-key &key (cpu-name 'cpu) (sp-name 'sp) (sp-2-name 'sp-2) (value-name 'value))
  (alist :bindings `((,sp-name (cpu-sp ,cpu-name))
		     (,sp-2-name (- ,sp-name 2))
		     (,value-name (,(cpu-register-accessor-name register-pair-key) ,cpu-name)))
	 :sp sp-2-name
	 :memory `(((1- ,sp-name) (hi-byte ,value-name))
		   (,sp-2-name (lo-byte ,value-name)))))

(defun pop-instr-spec (register-pair-key &key (cpu-name 'cpu) (memory-name 'memory) (sp-name 'sp) (sp+-name 'sp+))
  (alist :bindings `((,sp-name (cpu-sp ,cpu-name))
		     (,sp+-name (1+ ,sp-name)))
	 :sp `(+ ,sp-name 2)
	 register-pair-key `(combined-register (aref ,memory-name ,sp+-name)
					       (aref ,memory-name ,sp-name))))

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

(defun compile-alu-op (alu-op-key a-name data-name carry-bit-name)
  (ecase alu-op-key
    (:add `(logand #xff (+ ,a-name ,data-name)))
    (:adc `(logand #xff (+ ,a-name ,data-name ,carry-bit-name)))
    ((:cp :sub) `(logand #xff (- ,a-name ,data-name)))
    (:sbc `(logand #xff (- ,a-name ,data-name ,carry-bit-name)))
    (:and `(logand ,a-name ,data-name))
    (:or `(logior ,a-name ,data-name))
    (:xor `(logxor ,a-name ,data-name))))

(defun alu-op-flags (alu-op-key a-name data-name carry-bit-name result-name)
  (ecase alu-op-key
    (:add (alist :carry? `(bit-carry? 7 ,a-name ,data-name)
		 :half-carry? `(bit-carry? 3 ,a-name ,data-name)
		 :subtraction? nil
		 :zero? `(zerop ,result-name)))
    (:adc (alist :carry? `(bit-carry-cy? 7 ,a-name ,data-name ,carry-bit-name)
		 :half-carry? `(bit-carry-cy? 3 ,a-name ,data-name ,carry-bit-name)
		 :subtraction? nil
		 :zero? `(zerop ,result-name)))

    ((:cp :sub) (alist :carry? `(bit-borrow? 8 ,a-name ,data-name)
		       :half-carry? `(bit-borrow? 4 ,a-name ,data-name)
		       :subtraction? t
		       :zero? `(zerop ,result-name)))
    (:sbc (alist :carry? `(bit-borrow-cy? 8 ,a-name ,data-name ,carry-bit-name)
		 :half-carry? `(bit-borrow-cy? 4 ,a-name ,data-name ,carry-bit-name)
		 :subtraction? t
		 :zero? `(zerop ,result-name)))

    (:and (alist :zero? `(zerop ,result-name)
		 :half-carry? t
		 :carry? nil
		 :subtraction? nil))
    ((:or :xor) (alist :zero? `(zerop ,result-name)
		       :half-carry? nil
		       :carry? nil
		       :subtraction? nil))))

;; TODO: a data structure with the names?
(defun 8bit-alu-instr-spec (opcode alu-op-key contents-key &key 
							     (pc-name 'pc)
							     (cpu-name 'cpu)
							     (imm8-name 'imm8)
							     (imm16-name 'imm16)
							     (memory-name 'memory))
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
     (instr-spec-defaults :cpu-name cpu-name
			  :memory-name memory-name
			  :pc-name pc-name
			  :opcode opcode
			  :cycles cycles)
     (when imm8?
       (imm8-instr-spec :pc-name pc-name :memory-name memory-name :imm8-name imm8-name))
     (when imm16?
       (imm16-instr-spec :pc-name pc-name :memory-name memory-name :imm16-name imm16-name))
     (alist
      :bindings `((a (cpu-a cpu))
		  (data ,(compile-key-accessor contents-key))
		  ,@(when carry? `((cy (cpu-carry-bit ,cpu-name))))
		  (result ,(compile-alu-op alu-op-key 'a 'data 'cy)))
      :name (alu-name alu-op-key contents-key)
      :description (alu-description alu-op-key contents-key)
      :pc `(+ ,instr-size ,pc-name))
     (alu-op-flags alu-op-key 'a 'data 'cy 'result)
     (unless (eql :cp alu-op-key)
       (key-set-instr-spec :a 'result :cpu-name cpu-name :imm8-name imm8-name :imm16-name imm16-name)))))

(defun 8bit-inc-op-instr-spec (opcode inc-key contents-key &key 
							     (pc-name 'pc)
							     (cpu-name 'cpu)
							     (memory-name 'memory))
  (let* ((memory-ref? (member contents-key '(:@hl)))
	 (cycles (+ 1
		    (if memory-ref? 2 0)))
	 (instr-size 1)
	 (alu-op-key (ecase inc-key
		       (:inc :add)
		       (:dec :sub))))
    (merge-instr-specs
     (instr-spec-defaults :cpu-name cpu-name
			  :memory-name memory-name
			  :pc-name pc-name
			  :opcode opcode
			  :cycles cycles)
     (alist
      :bindings `((data ,(compile-key-accessor contents-key))
		  (result ,(compile-alu-op alu-op-key 'data 1 nil)))
      :name (list inc-key contents-key)
      :description (format nil "~A the contents of ~A."
			   (ecase inc-key
			     (:inc "Increments")
			     (:dec "Decrements"))
			   (key-description contents-key))
      :pc `(+ ,instr-size ,pc-name))
     (aremove (alu-op-flags alu-op-key 'data 1 'cy 'result) :carry?)
     (key-set-instr-spec contents-key 'result :cpu-name cpu-name))))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun instr-spec (opcode &key name description (cpu-name 'cpu) (memory-name 'memory) (pc-name 'pc) cycles
			      instr-size bindings disassembly)
    (amerge
     (alist :opcode opcode
	    :cpu-name cpu-name
	    :memory-name memory-name
	    :bindings (append `((,pc-name (cpu-pc ,cpu-name)))
			      bindings)
	    :name name
	    :description description
	    :disassembly disassembly
	    :cycles cycles)
     (when instr-size
       (alist :pc `(+ ,pc-name ,instr-size))))))

(defun bit-borrow-cy? (bit-index a b cy)
  ;; TODO: Check up on how to check for bit-borrow?
  (or (bit-borrow? bit-index a b)
      (bit-borrow? bit-index (- a b) cy)))


(defun half-borrow? (a b)
  ;; TODO: check if this is right.
  (let* ((half-a (logand #xf a))
	 (half-b (logand #xf b)))
    (< half-a half-b)))

;; S2.4
(definstr-class (#b00000011 (register-pair *register-pair-codes* 4)) (cpu memory)
  (let* ((combined-key (dd-register-pair-key register-pair)))
    `(((pc (cpu-pc cpu))
       (data (,(cpu-register-accessor-name combined-key) cpu)))
      :name ,(list :inc combined-key)
      :description ,(format nil "Increment the contents of register pair ~A" combined-key)
      ,combined-key (1+ data)
      :pc (1+ pc)
      :cycles 2)))

(defun bit7? (byte)
  (not (zerop (logand #x80 byte))))
(defun cpu-carry-bit (cpu)
  (if (cpu-carry? cpu) 1 0))

;; S2.5
;; Rotate left contents of A
(definstr #x17 (cpu memory) ((pc (cpu-pc cpu))
			     (a (cpu-a cpu))
			     (rla (rotate-left a (cpu-carry-bit cpu))))
	  :name :rla
	  :description "Rotate the contents of register A to the left,
through the carry (CY) flag."
	  :zero? nil
	  :subtraction? nil
	  :half-carry? nil
	  :carry? (bit7? a)
	  :cycles 1
	  :a rla
	  :pc (1+ pc))

(definstr-class (#b00010000 (register *register-codes* 0)) (cpu memory)
  (let ((register-key (register-key register)))
    `(((pc (cpu-pc cpu))
       (data (,(cpu-register-accessor-name register-key) cpu))
       (result (rotate-left data (cpu-carry-bit cpu))))
      :name ,(list :rl register-key)
      :description ,(format nil "Rotate the contents of register ~A to the left. 
That is, the contents of bit 0 are copied to bit 1,
and the previous contents of bit 1 are copied to bit 2.
The previous contents of the carry (CY) flag
are copied to bit 0 of register ~A." register-key register-key)
      :zero? (zerop result)
      :carry? (bit7? data)
      :subtraction? nil
      :half-carry? nil
      ,register-key result
      :pc (+ pc 2)
      :cycles 2)))

;; S2.6
;; Test complement of (bit of 8-bit register) into zero flag
;; Bit bit-index, r8
(deflong-instr-class (#b01000000 (bit-index *bit-indices* 3) (register-code *register-codes* 0)) (cpu memory)
  (let ((register-key (register-key register-code)))
    `(((pc (cpu-pc cpu))
       (bit-mask ,(expt 2 bit-index)))

      :name ,(list :bit bit-index register-key)
      :description ,(format nil "Copy the complement of the contents of bit ~A in
register ~A to the Zero flag." bit-index register-key)
      :zero? (zerop (logand (,(cpu-register-accessor-name register-key) cpu) bit-mask))
      :subtraction? nil
      :half-carry? t
      :pc (+ pc 2)
      :cycles 2)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *condition-codes* '(0 1 2 3))
  (defun compile-condition-code-test (condition-code cpu-name)
    (ecase condition-code
      ((0 :nz) `(not (cpu-zero? ,cpu-name)))
      ((1 :z) `(cpu-zero? ,cpu-name))
      ((2 :c) `(cpu-carry? ,cpu-name))
      ((3 :nc) `(not (cpu-carry? ,cpu-name))))))

;; S2.7
;; Jump relative on condition
(definstr-class (#b00100000 (condition-code *condition-codes* 3)) (cpu memory)
  `(((pc (cpu-pc cpu))
     (cc? ,(compile-condition-code-test condition-code 'cpu))
     (offset (s8 (aref memory (1+ pc))))
     (destination (+ 2 pc offset)))
    :name ,(list :jr condition-code :s8)
    :description ,(format nil "If the condition code ~A is true,
jump s8 steps from the current address stored in PC.
If not, the instruction following the current JP
instruction is executed (as usual)." condition-code)
    :disassembly (alist :s8 offset :addr (register16-text destination))
    :jump? cc?
    :pc (if cc?
	    destination
	    (+ 2 pc))
    :cycles (if cc? 3 2)))

;; S2.8
;; Call immediate
(definstr #xcd (cpu memory) ((pc (cpu-pc cpu))
			     (return-value (+ pc 3))
			     (a16 (combined-register (aref memory (+ 2 pc)) (aref memory (1+ pc))))
			     (sp (cpu-sp cpu)))
	  :name :call-a16
	  :description "Return value is PC+3.
Push hi-byte of return value. 
Push the lo-byte of return value.
Jump to A16.
To Push: decrement SP, then copy byte to SP."
	  :disassembly (alist :a16 (hex16-text a16))
	  :jump? t
	  :memory (((1- sp) (hi-byte return-value))
		   ((- sp 2) (lo-byte return-value)))
	  :sp (- sp 2)
	  :pc a16
	  :cycles 6)

;; Return
(definstr #xc9 (cpu memory) ((sp (cpu-sp cpu))
			     (return-addr (combined-register (aref memory (1+ sp)) (aref memory sp))))
	  :name :ret
	  :description "Pops the 16-bit return value off the stack into the PC."
	  :disassembly (alist :addr (hex16-text return-addr))
	  :jump? t
	  :sp (+ sp 2)
	  :pc return-addr
	  :cycles 4)

(defun rotate-left (register carry-bit)
  (logior carry-bit (logand #xff (ash register 1))))

(defun instr-effects (cpu memory)
  (let* ((instr (next-instr cpu memory)))
    (when instr
      (funcall (aval :instr-effects instr) cpu memory))))

(defun bit-value (value index)
  (let ((bitmask (ash 2 index)))
    (if (zerop (logand bitmask value))
	0
	1)))

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

(defbreakpoint past-first-loop (cpu memory)
  (let* ((pc (cpu-pc cpu)))
    (< #x0a pc)))


(defun compile-disassemble-instr-spec (instr-spec)
  (compile-disassemble-instr (aval :cpu-name instr-spec)
			     (aval :memory-name instr-spec)
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
		 (8bit-alu-instr-specs))))
  
  ;; TEMP: Add all single-byte instr-specs to instrs
  (mapcar (fn (asetq (aval :opcode %) % *instrs*)) (remove-if (fn (aval :long? %)) *instr-specs*))
  ;; TEMP: add all 2-byte instr-specs to instrs
  (mapcar (fn (asetq (aval :opcode %) % *long-instrs*)) (remove-if-not (fn (aval :long? %)) *instr-specs*))
  ;; Recompile the execute! function
  (eval (compile-execute)))

;; recompile execute! definition
(define-instrs!)

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


;; focus: the memory address that was last modified
;; add cycles
