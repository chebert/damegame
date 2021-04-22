(in-package #:8bit-transfers)

(install-syntax!)


(define (offset relative-address)
  "Offset the relative-address into the memory-registers."
  (+ #xFF00 relative-address))

(define (imm8 memory address)
  "Return the 8-bit immediate assuming address is at the opcode."
  (aref memory (1+ address)))
(define (imm16 memory address)
  "Return the 16-bit immediate assuming address is at the opcode."
  (u16-from-memory memory (1+ address)))

(define (relative-immediate memory address)
  "Returns the relative address specified by the immediate (n)."
  (offset (imm8 memory address)))

(define (u16-from-memory memory address)
  (u16 (aref memory (1+ address))
       (aref memory address)))

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

(define (extract-source opcode)
  "Extract the r-coded source register-name from the opcode"
  (r-code->register-name (bit-extract opcode 0 3)))
(define (extract-dest opcode)
  "Extract the r-coded destination register-name from the opcode"
  (r-code->register-name (bit-extract opcode 3 3)))


(define (remove-indices indices list)
  (nreverse (foldl (lambda (arg index result)
		     (if (member index indices)
			 result
			 (cons arg result)))
		   ()
		   list
		   (range (length list)))))

(define-examples remove-indices
  ((remove-indices '(1 3) '(a b c d e f)) '(a c e f)))

(define (ignore-args f . indices)
  (lambda args
    (apply f (remove-indices indices args))))

(define-examples ignore-args
  ([(ignore-args (lambda (a c d)
		   (list a c d))
		 1 4)
    :a :b :c :d :e]
   '(:A :C :D)))

(define (ignore-memory compiler) (ignore-args compiler 2 3))
(define (ignore-opcode compiler) (ignore-args compiler 1))
(define (just-machine compiler) (ignore-args compiler 1 2 3))

(define-examples ignore
  ([(ignore-memory (lambda (machine opcode)
		     (list machine opcode)))
    :machine :opcode :memory :address]
   '(:MACHINE :OPCODE))
  ([(ignore-opcode (lambda (machine memory address)
		     (list machine memory address)))
    :machine :opcode :memory :address]
   '(:MACHINE :memory :address))
  ([(just-machine (lambda (machine)
		    (list machine)))
    :machine :opcode :memory :address]
   '(:MACHINE)))

(register-compiler! #b01000000 nil
		    (ignore-memory
		     (lambda (machine opcode)
		       (compile-ld machine (extract-source opcode) (extract-dest opcode)))))

(register-compiler! #b00000110 nil
		    (lambda (machine opcode memory address)
		      (compile-ld machine (imm8 memory address) (extract-dest opcode) 2)))

(register-compiler! #b00001010 nil
		    (just-machine
		     (lambda (machine)
		       (compile-ld machine '(:bc) :a))))
(register-compiler! #b00011010 nil
		    (just-machine
		     (lambda (machine)
		       (compile-ld machine '(:de) :a))))

(register-compiler! #b11110010 nil
		    (just-machine
		     (lambda (machine)
		       (compile-ld machine '(:c) :a))))
(register-compiler! #b11100010 nil
		    (just-machine
		     (lambda (machine)
		       (compile-ld machine :a '(:c)))))

(register-compiler! #b11110000 nil
		    (ignore-opcode
		     (lambda (machine memory address)
		       (compile-ld machine (relative-immediate memory address) :a 2))))
(register-compiler! #b11100000 nil
		    (ignore-opcode
		     (lambda (machine memory address)
		       (compile-ld machine :a (relative-immediate memory address) 2))))

(register-compiler! #b11111010 nil
		    (ignore-opcode
		     (lambda (machine memory address)
		       (compile-ld machine (imm16 memory address) :a 3))))

(register-compiler! #b11101010 nil
		    (ignore-opcode
		     (lambda (machine memory address)
		       (compile-ld machine :a (imm16 memory address) 3))))

(uninstall-syntax!)

