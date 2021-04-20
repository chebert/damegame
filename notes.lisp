
;; How do I organize event-handlers


;; Option 1: I could have a list of...
;; *event-handlers*
;; '((event-control-clicked . ((control-id . (lambda () (setq *quit?* t)))

;; Option 2: matcher-fns
;; *event-handlers*
;;   given an event; call a match function.
#+nil
(list
 (cons
  (lambda (event) (and (event-control-clicked-p event)))
  (lambda (event) (setq *quit?* t)))
 (cons
  (lambda (event) (and (event-control-clicked-p event)))
  (lambda (event) (setq *quit?* nil))))

;; Advantages: most flexible and easy to implement.
;; Redundant matchers might be called, can't sort or organize.
;;   (unoptimized)

;; Option 3: matcher structs
;; like option 2 but structs describing the match are provided
;; to aid in organization, sorting and optimizing


;; option 4: just a table of event handlers
;; (alist id1 fn1 id2 fn2)
;; simplest to implement, just as flexible as Option 2.


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


;; This time (1/21):



;; Next goal:
;;   render a visualization of the current state of the gameboy CPU

;; CPU state:
;; List of flags:
;; Z: zero (set when the result of the operation is 0 or two values match in a compare operation)
;; N: subtraction flag (set if the last operation was a subtraction)
;; H: Half-carry (set if a carry occurred from the lower nibble/4-bits to the higher nibble)
;; C: Carry (set if a carry occurred/or A is smaller in a compare operation)
;; registers:
;; 8-bit registers:
;;  (15..8)  (7..0)
;;   High  |  Low
;;    A        F
;;    B        C
;;    D        E
;;    H        L
;; 16-bit registers:
;;  SP: stack pointer (initialized to 0xFFFE)
;;  PC: program counter (intialized to 0x100)


;; Ascii design of what I want the CPU layout to look like.

;;       Zero? [No]   Subtraction? [Yes]
;; Half-carry? [No]    Full-carry? [Yes]
;; A 0xFFAB  F 0xBCDE    AF 0xFFABBCDE
;; B 0xFFAB  C 0xBCDE    BC 0xFFABBCDE
;; D 0xFFAB  E 0xBCDE    DE 0xFFABBCDE
;; H 0xFFAB  L 0xBCDE    HL 0xFFABBCDE
;;                       SP 0xBABBABAB
;;                       PC 0x00000100

;; with alternative representations of bytes:
;;   represent as unsigned decimal; signed decimal; hexadecimal; binary; ascii characters; more?

;; Plan of attack;
;; Draw a static version; CHECK
;; Draw a version that is based on the CPU; CHECK
;; switch representation of bytes/numbers; CHECK

;; defhandler
;;   make it harder to forget to add event matcher clause
;;   make it harder to accidentally name handlers the same thing


;; This time (1/24)
;;  make buttons easier to define. CHECK
;;  make laying out things easier.
;;    draw mouse coordinates at the top left. CHECK
;;    allow me to redefine buttons at runtime. CHECK.
;;    specify positions in grid coordinates. CHECK.


;; show me what changed. CHECK
;;  show me which registers are going to change in green
;;  show me which registers were 'changed' in red
;;    - if a register will change and was changed: yellow

;; I need to know:
;;  what did the instruction try to change? (list of registers & flags)
;;    does this true?: sometimes the instruction conditionally changes a register/flag
;;    I don't think this is true
;;  which registers & flags actually changed?
;;    need to track the last cpu state

;;  show the previous state of the cpu.
;;    be able make a copy of the current cpu state
;;    draw a rectangle and title for the cpu


;; state-change data
;;   cpu flags
;;   registers
;;     distinguish between setting individual registers and combined registers
;;   memory-update: address, byte-value
;;   pc: jump vs next instruction
;;     if a jump occurs, where to?


;; state-change data => generate execute! code
;;  state-change holds a collection of functions



;; compile state-changes to:
;;  1) instruction execute!
;;  2) use for visualizations
;;     focus in on last changed memory location
;;     determine programattically what values will change


;; next thing to think about
;;   store state-change definitions
;;   use state-change definitions to enhance visualizations and replace some instr-data
;;     (lambda (cpu state) ...) => list of concrete state changes
;; e.g. calling (state-changes state-change-def cpu memory) =>
'((:memory (#x9fff #x80))
  (:hl #x9ffe)
  (:pc #x0007))


;; indicate the following in memory:
;;   if memory was just changed show what it was changed from & to


;; Opcode Template
;;   #b01000000
;; Opcode Parameters
;;   bit-index @ opcode-bit-3
;;   register-code @ opcode-bit-0
;; create all combinations of opcodes given list of all bit-indices and list of all register-codes
;; call definstr for that particular type for each opcode


;;  left OP a -> a

;; OP r
;;  1 cycle
;;  1 instr byte

;; OP n
;;  2 cycles
;;  2 instr bytes

;; OP (HL)
;;  2 cycles
;;  1 instr byte




;; Noticed duplication
;;  s-instructions; e.g.
;;     CP r
;;     CP n
;;     CP (HL)

;; ADD/SUB
;;   differences:
;;     bit-carry 7/bit-borrow 8
;;     bit-carry 3/bit-borrow 4
;;     descriptions
;;     names
;;     +/-

;; CP/SUB
;;   differences:
;;     result ignored/A <- result
;;     description
;;     name

;; IDEA:
(defun compile-8bit-sub-flags ()
  (alist :carry? `(bit-borrow? 8 left right)
	 :half-carry? `(bit-borrow? 4 left right)
	 :subtraction? t
	 :zero? `(zerop result)))
(defun compile-sub-bindings (a b)
  `((left ,a)
    (right ,b)
    (result (logand #xff (- left right)))))

(alist :opcode #xc9
       :cpu-name 'cpu
       :memory-name 'memory
       :bindings '((sp (cpu-sp cpu))
		   (return-addr (combined-register (aref memory (1+ sp)) (aref memory sp))))
       :name :ret
       :description "Pops the 16-bit return value off the stack into the PC."
       :disassembly '(alist :addr (hex16-text return-addr))
       :jump? t
       :sp '(+ sp 2)
       :pc 'return-addr
       :cycles 4)

#+nil
(definstr-spec (alist :opcode #xc9
		      :cpu-name 'cpu
		      :memory-name 'memory
		      :bindings '((sp (cpu-sp cpu))
				  (return-addr (combined-register (aref memory (1+ sp)) (aref memory sp))))
		      :name :ret
		      :description "Pops the 16-bit return value off the stack into the PC."
		      :disassembly '(alist :addr (hex16-text return-addr))
		      :jump? t
		      :sp '(+ sp 2)
		      :pc 'return-addr
		      :cycles 4))


#+nil
(definstr-spec-class
    #b10010000  
    (alist 0 *register-codes*)
  (lambda (opcode register)
    (let* ((register-key (register-key register))
	   (accessor (cpu-register-accessor-name register-key)))
      (merge-instr-specs
       (alist :opcode opcode
	      :cpu-name 'cpu
	      :memory-name 'memory
	      :bindings `((pc (cpu-pc cpu)))
	      :name (list :sub register-key)
	      :description (format nil "Subtracts the contents of register ~A from
register A and stores the result in register A" register-key)
	      :a 'result
	      :pc '(+ pc 1)
	      :cycles 1)
       (sub-8bit-instr-spec '(cpu-a cpu) `(,accessor cpu))))))



;; Code Smells:
;;   #1 eval-when everywhere
;;   #2 complicated macro expansion: macrolet???!???
;;   #3 can combine some of the alu opcodes: hl and registers hl is code #b110
;;   #4 alu-8bit-instr-spec have a lot of similarities
;; imm8-instr-spec, ld-8bit-instr-spec are setting/assuming too many variables names

;; GOAL #1:
;;   replace macros with function calls; should take care of #2
;;   create instructions inside of a function. should take care of #1


;; S1.2
;; Dot-matrix LCD
;; 64-kbit SRAM for LCD
;; 64-kbit SRAM working memory
;; 32-pin connector for cartrdiges
;; Sound amp
;; keys for operation (buttons)
;; Speaker


;; Interrupt Flags
;; IF (Interrupt Flag)                 #xFF0F
;; IE (Interrupt Enable)               #xFFFF
;; IME (Master Interrupt Enable)

;; 4 maskable interrupts
;;;  LCD Display Vertical Blanking
;;;  Status interrupts from LCDC (4 modes)
;;;  Timer Overflow Interrupt
;;;  Serial Transfer Completion Interrupt
;; 1 maskable external interrupt
;;;  End of input signal for ports P10-P13


;; DMA
;;; 40x32-bit transfers from 0x8000-0xDFFF to OAM memory (sprite memory)
;;; Horizontal-blanking DMA transfer: 16bytes per horizontal blanking automatically transferred
;;;   from user program or external working memory to the LCD RAM area
;;; General Purpose DMA transfer: 16-2048 bytes are transferred from user program
;;;   or external working memory to LCD RAM area during the vertical blanking period

;; Timer
;;; TIMA (counter)
;;; TMA (modulor register)
;;; TAC (control register)

;; Connections
;;; P10-P13: Input ports
;;; P14-P15: key matrix structure.

;; S2.4.4
;; IF (interrupt requested): xff0f
;;  bits7-5: unused
;;  bit4: P10-P13 terminal negative edge (button pressed?): #x0060
;;  bit3: Serial I/O terminal completion: #x0058
;;  bit2: timer overflow: #x0050
;;  bit1: LCDC (?): #x0048
;;    E.G. occurs when the video hardware is about to redraw a given LCD line
;;    called when a condition in the LCD Control register is met.
;;  bit0: vertical blanking: #x0040

;; IE (interrupt enabled): #xffff
;; IME: Interrupt Master enabled

;; Priority is from bit0->bit4

;; process
;; 1. IF flag is set
;; 2. if IME and corresponding IE flag is set:
;;    a. IME flag is reset (0)
;;    b. Push PC onto the stack
;;    c. Jump PC to starting address of interrupt.

;; interrupt processing routine pushes the registers
;; Return from interrupt using RETI and RET
;; RETI sets IME
;; Check for interrupts during the op-code fetch cycle of each instruction.



;; Display
;;; 160x144 pixel display
;; Tiles:
;;; 8x8 pixel grid; also called patterns
;; Palette:
;;; 4 grey colors
;;; 2-bits per pixel: index into palette
;; Layers:
;;; 3 layers (from bottom to top):
;;;   1) background: tilemap; can be scrolled as a whole
;;;   2) window: bg layer on top of the background; (e.g. used for fixed-position HUD elements)
;;;   3) sprites: tiles that can be displayed anywhere on screen

;; Tilemap:
;;; grid of references to tiles
;; Hardware Objects:
;;; 1 or 2 stacked tiles (8x8 or 8x16 pixels)
;; Metasprite:
;;; 2+ Hardware objects combined together to describe a single sprite (software)
;; Sprite:
;;; Collection of tiles used to display a single frame of character animation


;; LCD Control Register: #xff40
;;; bit - name - description
;;; 7 - LCD Display enable - controls whether the LCD is on; display is blank (whiter than white) when off;
;;; 6 - Window Tile Map Display Select - controls which tile map to use when drawing window tiles
;;; 5 - Window Display Enable - controls whether the window is displayed
;;; 4 - BG & Window Tile Data Select - controls which tile data should be used by background and window tile data
;;; 3 - BG Tile Map Display Select - controls which tile map to use when drawing background tiles
;;; 2 - Object Size - controls whether the hardware object is 1 or 2 stacked tiles
;;; 1 - Object Display Enable - controls whether or not sprites are displayed; can be toggled mid-frame
;;; 0 - BG & window Display - controls whether or not the background & window are displayed; become white if not displayed


;; We might need to be able to update the SDL display one scanline at a time (rather than the whole thing at once)
;; LCD can be controlled mid-scanline (but I don't think I want to support this)

;; LCD Status Register: #xff41
;;; bit - name -description
;;; 6 - Coincidence Interrupt -
;;; 5 - Mode 2 OAM (sprite attribute table) Interrupt -
;;; 4 - Mode 1 V-blank Interrupt -
;;; 3 - Mode 0 H-blank Interrupt -
;;; 2 - Coincidence Flag -
;;; 1,0 - Mode Flag -
;;;;  Mode 0: During H-blank: moving the pen to the start of the next scanline.
;;;;    (85-208 cycles depending on how long Mode 3 takes)
;;;;  Mode 1: During V-blank: moving the pen to the first scanline.
;;;;    (4560 cycles)
;;;;  Mode 2: During Searching OAM
;;;;    Scan for (X,Y) coordinates of sprites that overlap this scanline (80 cycles)
;;;;  Mode 3: During transferring data to the LCD driver
;;;;    Reading OAM and VRAM to generate scanline (168 to 291 cycles depending on sprite count)


;; => 5016

;;; During Mode 2-3: CPU cannot access OAM
;;; During Mode 3: CPU cannot access VRAM

;;; Pausing the Dot Clock
;;;;  background scrolling: at the start of the scanline pauses while discarding pixels from the first tile.
;;;;     pause for scroll-x MOD 8
;;;;  Window: pauses for at least 6 dots
;;;;  sprites: 11 - min(5, (x + scroll-x) MOD 8) if left side is over a background tile;
;;;;    if over a window tile, use window-x instead of scroll-x


;; 60 FPS

;;; Position and Scrolling
;;;; #xFF42: scroll-y
;;;; #xff43: scroll-x
;;;;   specifies the position into the 256x256 bg map which is displayed at the upper left
;;;;   automatically wraps up to the upper left when drawing exceeds the lower-right border
;;; #xFF44: LCDC Y coordinate
;;;; indicates the current scanline: 144-153 indicate v-blank mode
;;; #xFF45: LCDC Compare Y coordinate
;;;; Causes interrupt when this is equal to LCDC Y coordinate
;;; #xFF4A: Window Y position
;;; #xFF4B: Window X position (valid in range 7..165)
;;;;  same as scroll-y, scroll-x but for the window instead of the background.
;;;;  window becomes visible if in the range x=0..166 and y=0..143

;;; #xFF47: Background Palette
;;;; assigns shades of grey to the palette; 0: 0-1, 1: 2-3, 2: 4-5, 3: 6-7
;;; #xFF48: Object Palette 0 data
;;;; assigns shades of grey to the palette: 0: transparent, 1: 2-3, 2: 4-5, 3: 6-7
;;; #xFF49: Object Palette 1 Data
;;;; exactly like #xFF48

;;; LCD OMA DMA transfers
;;;; #xFF46: DMA transfer and Start address
;;;;   Begins DMA transfer from the range #xXX00 #xXX9F to #xFE00 #xFE9F
;;;;   takes 40 cycles
;;;;   CPU can only access HRAM (#xFF80 #xFFFE)
;;;;   Sprites are not displayed during this transfer


;;; VRAM Tile Data
;;;; One tile is 16 bytes
;;;; 8x8 pixels
;;;; color depth of 4 shades of grey
;;;; each 2-bytes represents a line
;;;;   Colors: B7A7 B6A6 ... B0A0 (left to right) 
;;; 3 blocks of tiles
;;;; 128 tiles each
;;;; 2 addressing modes: #x8000 + u8 or #x8800 + s8

;;; VRAM Background Maps
;;;; Background Tile Map
;;;;; 32x32 grid
;;;;; scrolled using scroll-x scroll-y
;;;; Window Tile Map
;;;;; location based on window-x and window-y (no scrolling)
;;;;; positioned at window-x - 7, window-y

;;; OAM Sprite Attribute Table
;;;; only 10 sprites per scanline
;;;; byte0: y position (sprites are offset -16 when drawn)
;;;; byte1: x position (sprites are offset -8 when drawn)
;;;; byte2: tile/pattern number
;;;;   in 8x8 mode: tile index
;;;;   in 8x16 mode: top    tile index is byte2 AND #xFE
;;;;                 bottom tile index is byte2 OR #x01
;;;; byte3: attributes/flags
;;;;   bit7 object-to-background priority (0=in front of bg; 1 = behind bg colors 1-3)
;;;;   bit6 y-flip (0=normal, 1=vertically mirrored)
;;;;   bit5 x-flip
;;;;   bit4 Palette number
;;; Sprite priority
;;;; During OAM scan, picks the first 10 sprites in each scanline
;;;; Highest priority sprites are displayed on top
;;;;   priority is chosen left to right, followed by first in memory

;; (scan-oam scanline-index memory)
;;; Create a priority queue of sprites intersecting with scanline
;;; return the number of cycles this operation took

;; (scanline-pixels scanline-index sprite-queue memory)
;;; Create a scanline of pixels based on OAM and VRAM
;;; Return the number of cycles this operation took

;; Copy pixels into pixel buffer

;; (h-blank!)
;;; Cause the h-blank interrupt to occur
;;; return the number of cycles in an h-blank

;; events for when LCD modes start/end; based on the number of cycles;
;; changes would be atomic and occur at the end event
;;; scan-oam start: (80 cycles, signal LY interrupt)
;;; scan-oam end: (create priority queue of sprites)
;;; scanline-pixels start: (x cycles)
;;; scanline-pixels end: (create the pixel buffer)
;;; h-blank start: (x cycles, signal h-blank interrupt)
;;; h-blank end: (start the next scanline or go to v-blank)
;;; ...
;;; v-blank start: (x cycles, signal v-blank interrupt, draw the new frame to the SDL texture)
;;; v-blank end: (start first scanline)

;;; dma transfer start: (x cycles)
;;; dma transfer end: (copy memory)


;; MBC1
;;;; x0000-x3fff:
;;;;   ROM Bank. Normally the first 16 Kb of the cart. Could be banks 20/40/60 in mode 1.
;;;; x4000-x7FFF:
;;;;   ROM Bank 01-7f. Any of the banks not-addressable by the first ROM bank area.
;;;; xA000-xBFFF:
;;;;   RAM Bank 00-03.
;;; Control Registers
;;;; x0000-x1fff:
;;;;   x0A: enable RAM
;;;;   x00: disable RAM
;;;; x2000-x3fff: (2nd) ROM bank number select. Defaults to 1.
;;;;   N: sets the number of banks to (logand #b11111 n).
;;;; x4000-x5fff: RAM bank number select.
;;;; x6000-x7fff: Banking Mode Select. Defaults to 0.
;;;;   0: Simple ROM Banking Mode
;;;;   1: RAM Banking Mode/Advanced ROM Banking mode.


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



;; Convert to defvis
;;   LCD-Debug
;;   memory update
;;   Game
;;   (combined) CPU view disassembly and previous disassembly
;;   instruction description
;;   interrupt enable states
;;   mouse position
;;   mouse cursor
;; Calculate the width/height of a visualization (to draw a box around it)
;; For disassembly: show nice text of the disassembly (text-from-disassembly)
;; organize the views some.

;; New views
;;   A list of last memory updates (and pcs and cycles they ocurred at)
;;   A list of last pcs (and cycles they ocurred at)
;;   A list of lcd mode changes (and cycles they ocurred at)
;;   A list of currently active breakpoints

;; Always want to see
;;    buttons to control the simulation
;;       add: continue to v-blank-start

;; Group views together:
;;   CPU:
;;     current cpu state
;;     Memory that is going to be updated
;;     previous cpu state
;;     disassembly
;;     memory updated
;;     buttons to change number base
;;     instruction description
;;     memory regions around PC, SP, and more?
;;   LCD Debug
;;     palettes as bits and colors
;;     tile-data blocks
;;     lcd control registers
;;     lcd status registers
;;     background tile-map
;;     window tile-map
;;     OAM
;;       tile-data drawn
;;       flags for OAM
;;     final LCD screen
;;   Cartridge:
;;     current rom-bank
;;     rom header data
;;   Game:
;;     final LCD screen
;;     current cycle


;; Draw relative: specify a percentage
;;   (of width/height) offset to draw a texture 



;; Entitiy System: mostly pure-functional

;;; A system of entities that are loosely coupled, and can communicate asynchronously via events.
;;;   event: an occurrence. may not be for anyone in particular

;;; (handle-event system entity event) => (new-entity . events)
;;;   system is the last state of the entities

;;; a system is a flat list of entities

;;; (dispatch-event system event) => system
;;;   each entity would handle event (in no particular order)
;;;   the events would be collated, and dispatched again until
;;;      - no more events are left to dispatch
;;;      - some event limit is met (to avoid infinite loops)

;;; (id entity) => unique id of the entity

(defun handle-result (entity events)
  (alist :entity entity
	 :events events))

;; several ways we could write handle-event
;;   (defgeneric handle-event (system entity event))
;;     => dispatch on both entity-type and event-type

#+nil
(defmethod handle-event (system (entity button) (event left-mouse-released))
  (if (and (button-active? system button) (mouse-inside? system (button-rect entity)))
      (handle-result (button-deactivated button) (list (button-click-event button)))
      (handle-result (button-deactivated button) '())))

;; second way to write handle-event
(defun handle-event (system entity event)
  (funcall entity :handle-event system event))

;; Not functional...
#+nil
(defun button-entity (rect)
  (let* ((active? nil)
	 (hovered? nil))
    (lambda (message &rest args)
      (ecase message
	(:handle-event
	 (let* ((system (first args))
		(event (second args)))
	   (cond
	     ((left-mouse-released? event)
	      (setq active? nil)
	      (list (button-click-event button))
	      '()))))))))

;; a third way to write handle-event (no generics)
#+nil
(defun handle-event (system entity event)
  (cond
    ((button? entity)
     (cond
       ((left-mouse-released? event)
	;;...
	)
       ;;...
       ))
    ;;...
    ))

;; Predicate dispatching (best of both worlds?)
#+nil
(define-generic handle-event (system entity event)
  (handle-result entity '()))

#+nil
(define-method handle-event (system (entity 'button?) (event 'left-mouse-released?))
  ;;...
  )

;; handle-event stores a
;;   table of predicate-list -> implementation
;;   & default implementation

;; (handle-event system entity event)
;;   maps over table looking to find the implementation that matches, and calls that
;;   otherwise calls default implementation

(defvar *add-method!* (gensym))
(defvar *get-methods* (gensym))
(defun make-generic (default-impl)
  (let* ((methods ()))
    (lambda (&rest args)
      (cond
	((eq (first args) *add-method!*)
	 (let* ((predicates (second args))
		(impl (third args)))
	   (push (cons predicates impl) methods)))
	((eq (first args) *get-methods*) (append methods (list (cons t default-impl))))
	(t (let* ((method (find-if (lambda (predicates)
				     (predicates-match-args? predicates args))
				   methods
				   :key 'car)))
	     (if method
		 (apply (cdr method) args)
		 (apply default-impl args))))))))

(defun predicates-match-args? (predicates args)
  (every (lambda (predicate arg)
	   (or (eq t predicate) (funcall predicate arg)))
	 predicates args))

(defmacro define-generic (name args &body default-impl)
  `(progn
     (setf (symbol-function ',name) (make-generic (lambda ,args ,@default-impl)))
     ',name))

(defun add-method! (generic-function predicates impl)
  (funcall generic-function *add-method!* predicates impl))
(defun get-methods (generic-function)
  (funcall generic-function *get-methods*))

(defmacro define-method (name args-and-predicates &body impl)
  `(add-method! ',name
		(list ,@(mapcar (lambda (arg-and-predicate)
				  (if (listp arg-and-predicate)
				      (second arg-and-predicate)
				      t))
				args-and-predicates))
		(lambda ,(mapcar
			  (lambda (arg-and-predicate)
			    (if (listp arg-and-predicate)
				(first arg-and-predicate)
				arg-and-predicate))
			  args-and-predicates)
		  ,@impl)))

(define-generic handle-event (system entity event)
  (handle-result (list :updated-with entity event)
		 '()))

(defun dispatch-event (system event)
  (let* ((handle-results (mapcar (lambda (entity)
				   (handle-event system entity event))
				 system))
	 (system2 (remove nil (mapcar (lambda (result) (aval :entity result))
				      handle-results)))
	 (events (apply 'append (mapcar (lambda (result) (aval :events result))
					handle-results))))
    (cons system2 events)))

(defparameter *dispatch-event-limit* 100000)
(defun dispatch-events (system events)
  (loop while events
	for i below *dispatch-event-limit* do
	  (let* ((result
		   (reduce (lambda (result event)
			     (let* ((result2 (dispatch-event (car result) event)))
			       (cons (car result2) (append (cdr result) (cdr result2)))))
			   events
			   :initial-value (cons system '()))))
	    (setq system (car result)
		  events (cdr result))))
  (cons system events))


(dispatch-events '(:e1 :e2 :e3) '(:ev1 :ev2 :ev3))



;; Sub-instructions
(append
 ;; <value>: ((reg reg-name))
 ;;          ((const val))
 ;;          ((op op-name) . op-args)
 ;;          ((fetch val))
 ;; <addr>: (reg reg-name)
 ;;         (const value)
 ;; <target>: (reg reg-name)
 ;;           (high reg-name)
 ;;           (low reg-name)
 ;;           (flag flag-name)
 ;; <condition>: flag-name
 ;;              (not flag-name)
 ;; <dest>: ((const val)
 ;;          (reg reg-name))
 
 ;; (assign <target> <value>)
 ;; (flag flag-name <value>)
 ;; (mem-set <addr> <value>)
 ;; (goto <dest>)
 ;; (branch <condition> <dest>)
 ;; (advance-pc)
 (ld-8bit-instr-specs)
 ;; (assign reg-name <value>)

 ;; PUSH:
 ;;   (assign (reg sp) (op 1-) sp)
 ;;   (mem-set (reg sp) (op high-byte) <value>)
 ;;   (assign (reg sp) (op 1-) sp)
 ;;   (mem-set (reg sp) (op low-byte) <value>)
 
 ;; POP:
 ;;   (assign (high reg-name) (fetch (reg sp)))
 ;;   (assign sp (op 1+) sp)
 ;;   (assign (low reg-name) (fetch (reg sp)))
 ;;   (assign sp (op 1+) sp)

 ;; LDHL SP,s8
 ;;   (flag subtraction? (const nil)) 
 ;;   (flag zero? (const nil)) 
 ;;   (flag carry? (op carry-16?) (reg sp) (const s8)) 
 ;;   (flag half-carry? (op half-carry16?) (reg sp) (const s8)) 
 ;;   (assign (reg hl) (op +) (const s8) (reg sp))
 (ld-16bit-instr-specs)
 ;; ADD b
 ;;   (flag subtraction? (const nil))
 ;;   (flag carry? (op carry8?) (reg a) (reg b))
 ;;   (flag half-carry? (op half-carry8?) (reg a) (reg b))
 ;;   (assign (reg a) (op +) (reg a) (reg b))
 ;;   (flag zero? (op zero?) (reg a))

 ;; XOR l
 ;;   (flag subtraction? (const nil))
 ;;   (flag carry? (const nil))
 ;;   (flag half-carry? (const nil))
 ;;   (assign (reg a) (op xor) (reg a) (reg l))
 ;;   (flag zero? (op zero?) (reg a))
 (8bit-alu-instr-specs)

 ;; ADD HL, SP
 ;;   (flag subtraction? (const nil)))
 ;;   (flag carry? (op carry16?) (reg hl) (reg data))
 ;;   (flag half-carry? (op half-carry16?) (reg hl) (reg data))
 ;;   (assign (reg hl) (op +s16) (reg hl) (reg sp))
 ;;   (flag zero? (op zero?) (reg hl))
 (16bit-alu-op-instr-specs)
 (rotate-shift-instr-specs)
 (bit-instr-specs)

 ;; JP imm16
 ;;   (assign (low imm16) (op pc-byte))
 ;;   (advance-pc)
 ;;   (assign (high imm16) (op pc-byte))
 ;;   (advance-pc)
 ;;   (goto (reg imm16))
 ;; JP not-Carry
 ;;   (assign (low imm16) (op pc-byte))
 ;;   (advance-pc)
 ;;   (assign (high imm16) (op pc-byte))
 ;;   (advance-pc)
 ;;   (branch (not carry?) (reg imm16))
 ;; JP Carry?
 ;;   (assign (low imm16) (op pc-byte))
 ;;   (advance-pc)
 ;;   (assign (high imm16) (op pc-byte))
 ;;   (advance-pc)
 ;;   (branch carry? (reg imm16))
 ;; JP (HL)
 ;;   (goto (reg hl))
 ;; JR imm8
 ;;   (assign imm16 (const 0))
 ;;   (assign (low imm16) (op pc-byte))
 ;;   (advance-pc)
 ;;   (goto (op +) (reg imm16) (reg pc))
 (jump-instr-specs)

 ;; Call imm16
 ;;   (assign (low imm16) (op pc-byte))
 ;;   (advance-pc)
 ;;   (assign (high imm16) (op pc-byte))
 ;;   (advance-pc)
 ;;   (assign (reg sp) (op 1-) sp)
 ;;   (mem-set (reg sp) (op high-byte) (reg pc))
 ;;   (assign (reg sp) (op 1-) sp)
 ;;   (mem-set (reg sp) (op low-byte) (reg pc))
 ;;   (goto (reg imm16))

 ;; Ret
 ;;   (assign (high imm16) (fetch (reg sp)))
 ;;   (assign sp (op 1+) sp)
 ;;   (assign (low imm16) (fetch (reg sp)))
 ;;   (goto (reg imm16))
 (call-and-return-instr-specs)
 (general-purpose-arithmetic-instr-specs))
