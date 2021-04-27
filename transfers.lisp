(in-package #:transfers)

(install-syntax!)

(define (offset relative-address)
  "Offset the relative-address into the memory-registers."
  (+ #xFF00 relative-address))

(define (compile-getter machine spec)
  "Return a (lambda ()) that fetches the byte for the given spec.
Spec := register-name | u8 | (wide-register-name) | (u16) | (:C)"
  (cond
    ((list? spec)
     ;; Examples: '(C) '(BC) '(#xDEAD) 
     (let ((value (first spec))
	   (get-byte [machine :get-byte]))
       (cond
	 ((number? value) (lambda () [get-byte value]))
	 (t (let ((get (register-getter machine value)))
	      (if (eq? value :c)
		  (lambda () [get-byte (offset [get])])
		  (lambda () [get-byte [get]])))))))
    ((number? spec) (lambda () spec))
    (t (register-getter machine spec))))

(define (compile-setter machine spec)
  "Return a (lambda (value)) that sets the byte for the given spec.
Spec := register-name | (wide-register-name) | (u16) | (:C)"
  (cond
    ((list? spec)
     (let ((address (first spec))
	   (set-byte [machine :set-byte!]))
       (cond
	 ((number? address) (lambda (value) [set-byte address value]))
	 (t 
	  (let ((get (register-getter machine address)))
	    (if (eq? address :c)
		(lambda (value) [set-byte (offset [get]) value])
		(lambda (value) [set-byte [get] value])))))))
    (t (register-setter machine spec))))

(define (compile-ld machine source-spec dest-spec (instr-size 1))
  "Compiles a LD op for the given specs."
  (let ((get (compile-getter machine source-spec))
	(set (compile-setter machine dest-spec))
	(advance-pc [machine :advance-pc!]))
    (lambda ()
      [advance-pc instr-size]
      [set [get]])))

(define (compile-update-hl machine op)
  "Compiles to a function that updates HL using op. Not a full instruction."
  (let ((get-hl (register-getter machine :hl))
	(set-hl (register-setter machine :hl)))
    (lambda () [set-hl [op [get-hl]]])))

(define (ld-hl-update-compiler op source destination)
  "Returns a compiler that performs something like LD A, (HLI)."
  (just-machine
   (lambda (machine)
     (compose
      ;; It is important to update-hl AFTER performing the LD.
      (compile-update-hl machine op)
      (compile-ld machine source destination 1)))))

(define (extract-source opcode)
  "Extract the r-coded source register-name from the opcode"
  (r-code->register-name (bit-extract opcode 0 3)))
(define (extract-dest opcode)
  "Extract the r-coded destination register-name from the opcode"
  (r-code->register-name (bit-extract opcode 3 3)))


;; LD r, r'
(register-compiler! #b01000000 nil
		    (ignore-immediates
		     (lambda (machine opcode)
		       (compile-ld machine (extract-source opcode) (extract-dest opcode)))))

;; LD r, n
(register-compiler! #b00000110 nil
		    (ignore-immediate16
		     (lambda (machine opcode immediate8)
		       (compile-ld machine immediate8 (extract-dest opcode) 2))))

;;; Load contents of register pair
;; LD A, (BC)
(register-compiler! #b00001010 nil
		    (just-machine
		     (lambda (machine) (compile-ld machine '(:bc) :a))))
(register-compiler! #b00000010 nil
		    (just-machine
		     (lambda (machine) (compile-ld machine :a '(:bc)))))

(register-compiler! #b00011010 nil
		    (just-machine
		     (lambda (machine) (compile-ld machine '(:de) :a))))
(register-compiler! #b00010010 nil
		    (just-machine
		     (lambda (machine) (compile-ld machine :a '(:de)))))


;;; Load contents of memory registers
;; LD A, (C)
(register-compiler! #b11110010 nil
		    (just-machine
		     (lambda (machine) (compile-ld machine '(:c) :a))))
(register-compiler! #b11100010 nil
		    (just-machine
		     (lambda (machine) (compile-ld machine :a '(:c)))))

;; LD A, (n)
(register-compiler! #b11110000 nil
		    (just-immediate8
		     (lambda (machine immediate8)
		       (compile-ld machine (offset immediate8) :a 2))))
(register-compiler! #b11100000 nil
		    (just-immediate8
		     (lambda (machine immediate8)
		       (compile-ld machine :a (offset immediate8) 2))))

;;; Load contents of memory
;; LD A, (nn)
(register-compiler! #b11111010 nil
		    (just-immediate16
		     (lambda (machine immediate16)
		       (compile-ld machine immediate16 :a 3))))

(register-compiler! #b11101010 nil
		    (just-immediate16
		     (lambda (machine immediate16)
		       (compile-ld machine :a immediate16 3))))

;;; Load contents of HL and update
(register-compiler! #b00101010 nil (ld-hl-update-compiler #'1+ '(:hl) :a))
(register-compiler! #b00111010 nil (ld-hl-update-compiler #'1- '(:hl) :a))

(register-compiler! #b00100010 nil (ld-hl-update-compiler #'1+ :a '(:hl)))
(register-compiler! #b00110010 nil (ld-hl-update-compiler #'1- :a '(:hl)))


;;; 16bit-transfers

;; LD dd, nn
(register-compiler! #b00000001 nil
		    (ignore-immediate8
		     (lambda (machine opcode immediate16)
		       (let ((register-name (extract-dd-register-name opcode)))
			 (compile-ld machine immediate16 register-name 3)))))

;; LD SP, HL
(register-compiler! #b11111001 nil
		    (just-machine
		     (lambda (machine) (compile-ld machine :hl :sp))))

;; LD (nn), SP
(register-compiler! #b00001000 nil
		    (just-immediate16
		     (lambda (machine immediate16)
		       (compile-ld machine :sp (list immediate16) 3))))

;; LDHL
(register-compiler! #b11111000 nil
		    (just-immediate8
		     (lambda (machine immediate8)
		       (let ((set-f (register-setter machine :f))
			     (get-sp (register-getter machine :sp))
			     (set-hl (register-setter machine :hl))
			     (e (u8->s8 immediate8))
			     (advance-pc [machine :advance-pc!]))
			 (lambda ()
			   [advance-pc 2]
			   (let ((sp [get-sp]))
			     (let ((result (+ sp e)))
			       [set-f (flags nil nil (half-carry16? sp e) (carry16? sp e))]
			       [set-hl result])))))))

;; PUSH qq
(register-compiler! #b11000101 nil
		    (ignore-immediates
		     (lambda (machine opcode)
		       (let ((get-sp (register-getter machine :sp))
			     (set-sp (register-setter machine :sp))
			     (set-byte [machine :set-byte!])
			     (get-qq (register-getter machine (extract-qq-register-name opcode)))
			     (advance-pc [machine :advance-pc!]))
			 (lambda ()
			   [advance-pc]
			   (let ((sp [get-sp])
				 (qq [get-qq]))
			     (let ((sp-2 (- sp 2)))
			       [set-byte (1- sp) (high-byte qq)]
			       [set-byte sp-2 (high-byte qq)]
			       [set-sp sp-2])))))))
;; POP qq
(register-compiler! #b11000001 nil
		    (ignore-immediates
		     (lambda (machine opcode)
		       (let ((get-sp (register-getter machine :sp))
			     (set-sp (register-setter machine :sp))
			     (get-byte [machine :get-byte])
			     (set-qq (register-setter machine (extract-qq-register-name opcode)))
			     (advance-pc [machine :advance-pc!]))
			 (lambda ()
			   [advance-pc]
			   (let ((sp [get-sp]))
			     [set-qq (u16 [get-byte (1+ sp)] [get-byte sp])]
			     [set-sp (+ sp 2)]))))))


;; todo: (register-immediate8-compiler!) (register-immediate16-compiler!)
;; TODO:
;; BYTES      -> Assembly     -> Machine-assembly -> compiled opcode
;; #b11000001 -> (POP :BC)    -> ((assign :C '(:SP)) (assign :SP #'1+ :sp) (assign :B '(:SP)) (assign :sp #'1+ :sp)) -> (lambda ())
;; #b11111000
;; e-2        -> (LDHL :SP e) -> ((flag :zero? nil) (flag :subtraction? nil) (flag :half-carry? #'half-carry16? :sp e) (flag :carry? #'carry16? :sp e) (assign :hl #'+ sp e))

;; Machine-assembly
;;   (assign reg-name value)
;;     value := number |
;;              register-name |
;;              (register-name) |
;;              (number)
;;   (assign reg-name operation . values)
;;   (flag flag-name t)
;;   (flag flag-name nil)
;;   (flag flag-name op . flag-operands)
;;     flag-operand := number |
;;                     register-name

(uninstall-syntax!)
