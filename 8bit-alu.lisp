(in-package #:8bit-alu)

(install-syntax!)

;;; 8-bit Arithmetic: ADD, ADC, SUB, SBC

(define (add-flags result a data (carry 0))
  "Return the flags register with the appropriate flags set/reset for addition."
  (flags (zero? result) nil (half-carry8? a data carry) (carry8? a data carry)))
(define (subtract-flags result a data (carry 0))
  "Return the flags register with the appropriate flags set/reset for subtraction."
  (flags (zero? result) t (half-borrow8? a data carry) (borrow8? a data carry)))

(define ((perform-arithmetic op op-flags) get-a set-a get-f set-f data)
  "Performs arithmetic operation [OP a data] setting flags to [OP-FLAGS result a data]."
  (declare (ignore get-f))
  (let ((a [get-a]))
    (let ((result (u8 [op a data])))
      [set-a result]
      [set-f [op-flags result a data]])))

(define ((perform-arithmetic-with-carry op op-flags) get-a set-a get-f set-f data)
  "Performs arithmetic operation [OP a data carry] setting flags to [OP-FLAGS result a data carry]."
  (let ((a [get-a])
	(f [get-f]))
    (let ((carry (carry-bit f)))
      (let ((result (u8 [op a data carry])))
	[set-a result]
	[set-f [op-flags result a data carry]]))))

(define perform-add (perform-arithmetic #'+ #'add-flags))
(define perform-add-with-carry (perform-arithmetic-with-carry #'+ #'add-flags))
(define perform-subtract (perform-arithmetic #'- #'subtract-flags))
(define perform-subtract-with-carry (perform-arithmetic-with-carry #'- #'subtract-flags))

;;; 8-bit logical: AND, OR, XOR, CP

(define ((perform-logical op half-carry?) get-a set-a get-f set-f data)
  "Performs logical operation [op a data] setting flags appropriately."
  (declare (ignore get-f))
  (let ((a [get-a]))
    (let ((result [op a data]))
      [set-a result]
      [set-f (flags (zero? result) nil half-carry? nil)])))

(define perform-and (perform-logical #'logand t))
(define perform-or (perform-logical #'logior nil))
(define perform-xor (perform-logical #'logxor nil))

(define (perform-compare get-a set-a get-f set-f data)
  "Performs subtraction, setting the flags, but discarding the result."
  (declare (ignore get-f set-a))
  (let ((a [get-a]))
    [set-f (subtract-flags (- a data) a data)]))

(define-examples perform-add
  ((let ((a (make-register 'a 1 #xFF))
	 (flags (make-register 'f 1)))
     (perform-add [a :get-contents] [a :set-contents!] [flags :get-contents] [flags :set-contents!] 2)
     (list [[a :get-contents]]
	   (flags->list [[flags :get-contents]])))
   '(1 (:HALF-CARRY? :CARRY?))))

(define ((alu-op-compiler perform) machine opcode immediate8 immediate16)
  "Return a compiler that compiles an r-coded opcode (8-bit register or (HL)).
Calls [perform get-a set-a get-f set-f data]."
  (declare (ignore immediate8 immediate16))
  (let ((r-code (bit-extract opcode 0 3)))
    (let ((get-a (register-getter machine 'a))
	  (set-a (register-setter machine 'a))
	  (get-f (register-getter machine 'f))
	  (set-f (register-setter machine 'f))
	  (advance-pc [machine :advance-pc!])
	  (get-data (r-code->getter machine r-code)))
      (lambda ()
	[advance-pc]
	[perform get-a set-a get-f set-f [get-data]]))))

(define ((alu-immediate8-op-compiler perform) machine opcode immediate8 immediate16)
  "Return a compiler that compiles an immediate8 instruction"
  (declare (ignore opcode immediate16))
  (let ((get-a (register-getter machine 'a))
	(set-a (register-setter machine 'a))
	(get-f (register-getter machine 'f))
	(set-f (register-setter machine 'f))
	(advance-pc [machine :advance-pc!]))
    (lambda ()
      [advance-pc 2]
      [perform get-a set-a get-f set-f immediate8])))

;;; 8-bit updates: INC/DEC

(define (inc-flags result data flags)
  "Return the flags register value with the appropriate flags set/reset for INC."
  (flags (zero? result) nil (half-carry8? data 1 0) (carry-set? flags)))
(define (dec-flags result data flags)
  "Return the flags register value with the appropriate flags set/reset for DEC."
  (flags (zero? result) t (half-borrow8? data 1 0) (carry-set? flags)))

(define ((perform-update op op-flags) get-data set-data get-f set-f)
  "Performs an in-place update on data using [op data] and [op-flags result data f]."
  (let ((data [get-data])
	(f [get-f]))
    (let ((result (u8 [op data])))
      [set-f [op-flags result data f]]
      [set-data result])))

(define perform-inc (perform-update #'1+ #'inc-flags))
(define perform-dec (perform-update #'1- #'dec-flags))

(define ((update-op-compiler perform) machine opcode immediate8 immediate16)
  "Return a compile that compiles an INC/DEC update instruction."
  (declare (ignore immediate8 immediate16))
  (let ((r-code (bit-extract opcode 3 3)))
    (let ((get-data (r-code->getter machine r-code))
	  (set-data (r-code->setter machine r-code))
	  (get-f (register-getter machine 'f))
	  (set-f (register-setter machine 'f))
	  (advance-pc [machine :advance-pc!]))
      (lambda ()
	[advance-pc]
	[perform get-data set-data get-f set-f]))))

;; Register 8-bit compilers

(register-compiler! #b10000000 nil (alu-op-compiler #'perform-add))
(register-compiler! #b11000110 nil (alu-immediate8-op-compiler #'perform-add))
(register-compiler! #b10001000 nil (alu-op-compiler #'perform-add-with-carry))
(register-compiler! #b11001110 nil (alu-immediate8-op-compiler #'perform-add-with-carry))
(register-compiler! #b10010000 nil (alu-op-compiler #'perform-subtract))
(register-compiler! #b11010110 nil (alu-immediate8-op-compiler #'perform-subtract))
(register-compiler! #b10011000 nil (alu-op-compiler #'perform-subtract-with-carry))
(register-compiler! #b11011110 nil (alu-immediate8-op-compiler #'perform-subtract-with-carry))

(register-compiler! #b10100000 nil (alu-op-compiler #'perform-and))
(register-compiler! #b11100110 nil (alu-immediate8-op-compiler #'perform-and))
(register-compiler! #b10110110 nil (alu-op-compiler #'perform-or))
(register-compiler! #b11110110 nil (alu-immediate8-op-compiler #'perform-or))
(register-compiler! #b10101000 nil (alu-op-compiler #'perform-xor))
(register-compiler! #b11101110 nil (alu-immediate8-op-compiler #'perform-xor))

(register-compiler! #b10111000 nil (alu-op-compiler #'perform-compare))
(register-compiler! #b11111110 nil (alu-immediate8-op-compiler #'perform-compare))

(register-compiler! #b00000100 nil (update-op-compiler #'perform-inc))
(register-compiler! #b00000101 nil (update-op-compiler #'perform-dec))

(uninstall-syntax!)
