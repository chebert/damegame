(in-package #:16bit-alu)

(install-syntax!)

(define (ss-code->register-name ss-code)
  "Return the register or register-pair name associated with ss-code extracted from an opcode."
  (cond
    ((= ss-code #b00) :bc)
    ((= ss-code #b01) :de)
    ((= ss-code #b10) :hl)
    ((= ss-code #b11) :sp)
    (t (error "Unrecognized SS-CODE: ~S" ss-code))))

(define (opcode->ss-register opcode)
  (ss-code->register-name (bit-extract opcode 4 2)))



(define (perform-add-hl get-hl set-hl get-data get-f set-f)
  (let ((hl [get-hl])
	(data [get-data])
	(f [get-f]))
    (let ((result (value->u16 (+ hl data))))
      [set-hl result]
      [set-f (flags (zero-set? f) nil (carry16? hl data) (half-carry16? hl data))])))

(define (compile-add-hl machine opcode)
  (let ((ss-register (opcode->ss-register opcode)))
    (let ((get-hl (register-getter machine :hl))
	  (set-hl (register-setter machine :hl))
	  (get-data (register-getter machine ss-register))
	  (get-f (register-getter machine :f))
	  (set-f (register-setter machine :f))
	  (advance-pc [machine :advance-pc!]))
      (lambda ()
	[advance-pc]
	(perform-add-hl get-hl set-hl get-data get-f set-f)))))


(define (perform-add-sp get-sp set-sp data set-f)
  (let ((sp [get-sp]))
    (let ((result (value->u16 (+ sp data))))
      [set-sp result]
      [set-f (flags nil nil (half-carry16? sp data) (carry16? sp data))])))

(define (compile-add-sp machine memory address)
  (let ((get-sp (register-getter machine :sp))
	(set-sp (register-setter machine :sp))
	(set-f (register-setter machine :f))
	(advance-pc [machine :advance-pc!])
	(data (aref memory (1+ address))))
    (lambda ()
      [advance-pc 2]
      (perform-add-sp get-sp set-sp data set-f))))


(define ((perform-update op) get-data set-data)
  [set-data [op [get-data]]])

(define perform-inc (perform-update #'1+))
(define perform-dec (perform-update #'1-))

(define ((compile-update perform) machine opcode)
  (let ((ss-register (opcode->ss-register opcode)))
    (let ((get-data (register-getter machine ss-register))
	  (set-data (register-setter machine ss-register))
	  (advance-pc [machine :advance-pc!]))
      (lambda ()
	[advance-pc]
	[perform get-data set-data]))))


(define ((discard-memory compiler) machine opcode memory address)
  (declare (ignore memory address))
  [compiler machine opcode])
(define ((discard-opcode compiler) machine opcode memory address)
  (declare (ignore opcode))
  [compiler machine memory address])

(register-compiler! #b00001001 nil (discard-memory #'compile-add-hl))
(register-compiler! #b11101000 nil (discard-opcode #'compile-add-sp))
(register-compiler! #b00000011 nil (discard-memory (compile-update #'perform-inc)))
(register-compiler! #b00001011 nil (discard-memory (compile-update #'perform-dec)))

(uninstall-syntax!)