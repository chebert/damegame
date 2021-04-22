(in-package #:machine)

(install-syntax!)

(define (perform-push get-sp set-sp reg16 set-byte)
  "Pushes the two bytes in the 16-bit register reg16 onto the stack."
  (let ((sp [get-sp]))
    (let ((sp-2 (- sp 2)))
      [set-byte (1- sp) (high-byte reg16)]
      [set-byte sp-2 (low-byte reg16)]
      [set-sp sp-2])))

(define (perform-pop get-sp set-sp set-reg16 get-byte)
  "Pops two bytes off of the stack and sets the 16-bit register."
  (let ((sp [get-sp]))
    (let ((sp+1 (1+ sp)))
      [set-reg16 (u16 [get-byte sp+1] [get-byte sp])]
      [set-sp (1+ sp+1)])))

(define (flag-getter machine flag-name)
  "Return a getter which retrieves the current flag value as a boolean."
  (let ((get-flags (register-getter machine 'f))
	(flag-mask (flag-mask flag-name)))
    (lambda ()
      (not (zero? (logand flag-mask [get-flags]))))))

;; Opcodes
(define (r-code->register-name code)
  "Given an an r-code extracted from an opcode, return the register-name or '(hl)."
  (cond
    ((= code #b111) 'a)
    ((= code #b000) 'b)
    ((= code #b001) 'c)
    ((= code #b010) 'd)
    ((= code #b011) 'e)
    ((= code #b100) 'h)
    ((= code #b101) 'l)
    ((= code #b110) '(hl))
    (t (error "R-CODE->REGISTER-NAME: don't recognize code ~S" code))))


(export
 (define (r-code->getter machine code)
   "Return a compiled (lambda () byte) that fetches the byte associated with the given r-code."
   (let ((name (r-code->register-name code)))
     (if (equal? name '(hl))
	 (let ((get-hl (register-getter machine 'hl))
	       (get-byte [machine :get-byte]))
	   (lambda () [get-byte [get-hl]]))
	 (register-getter machine name)))))
(export
 (define (r-code->setter machine code)
   "Return a compiled (lambda (byte)) that sets the byte associated with the given r-code."
   (let ((name (r-code->register-name code)))
     (if (equal? name '(hl))
	 (let ((get-hl (register-getter machine 'hl))
	       (set-byte [machine :set-byte!]))
	   (lambda () [set-byte [get-hl]]))
	 (register-setter machine name)))))


;; Compilers


(define (compile-inc16 machine reg-name)
  (let ((get-register (register-getter machine reg-name))
	(set-register (register-setter machine reg-name))
	(advance-pc [machine :advance-pc!]))
    (lambda ()
      [advance-pc]
      [set-register (value->u16 (1+ [get-register]))])))

(define (compile-branch machine flag-name flag-set? memory address)
  (let ((destination (u16 (aref memory (+ 2 address)) (aref memory (1+ address))))
	(get-flag (flag-getter machine flag-name))
	(jump [machine :jump!])
	(advance-pc [machine :advance-pc!]))
    (lambda ()
      [advance-pc 3]
      (when (eq? flag-set? [get-flag])
	[jump destination]))))

(define (compile-push machine reg-name)
  (let ((get-sp (register-getter machine 'sp))
	(set-sp (register-setter machine 'sp))
	(set-byte [machine :set-byte!])
	(get-data (register-getter machine reg-name))
	(advance-pc [machine :advance-pc!]))
    (lambda ()
      [advance-pc]
      (perform-push get-sp set-sp [get-data] set-byte))))

(define (compile-pop machine reg-name)
  (let ((get-sp (register-getter machine 'sp))
	(set-sp (register-setter machine 'sp))
	(set-byte [machine :set-byte!])
	(set-data (register-setter machine reg-name))
	(advance-pc [machine :advance-pc!]))
    (lambda ()
      [advance-pc]
      (perform-pop get-sp set-sp set-data set-byte))))

(export
 (define (register-getter machine register-name)
   "Returns (lambda ()) which gets the current value of the register directly."
   [[[machine :get-register] register-name] :get-contents]))
(export
 (define (register-setter machine register-name)
   "Returns (lambda (value)) which sets the current-value of the register directly. "
   [[[machine :get-register] register-name] :set-contents!]))

(export
 (define rom? (make-bundle-predicate :rom)))
(export
 (define (make-rom (memory) (execution-procs))
   "Makes a region of read-only memory which can be compiled using
compile-execution-proc! and executed using get-execution-proc, or bytes
 of memory can be retrieved using get-byte."
   (define (initialize-memory! memory-vector)
     (set! memory memory-vector)
     (set! execution-procs (make-array (length memory))))

   (define (get-byte address)
     (aref memory address))

   (define (compile-execution-proc! machine address)
     (let ((proc (compile-execution-proc machine memory address)))
       (setf (aref execution-procs address) proc)
       :done))

   (define (get-execution-proc address)
     (aref execution-procs address))
   
   (bundle 'rom? (make-rom memory execution-procs)
	   initialize-memory!
	   get-byte
	   get-execution-proc
	   compile-execution-proc!)))

(export
 (define register? (make-bundle-predicate :register)))
(export
 (define (make-register symbol size-in-bytes (contents 0))
   "Create a CPU register which is either 1 or 2 bytes in size."
   (define (get-contents) contents)

   (define (set-contents8! value)
     (set! contents (u8 value)))
   (define (set-contents16! value)
     (set! contents (value->u16 value)))

   (define set-contents!
     (cond ((= size-in-bytes 2) set-contents16!)
	   ((= size-in-bytes 1) set-contents8!)
	   (t (error "REGISTER: ~S is not a valid size-in-bytes. Expected 1 or 2." size-in-bytes))))

   (bundle 'register? (make-register symbol size-in-bytes contents)
	   get-contents
	   set-contents!)))

(export
 (define machine? (make-bundle-predicate :machine)))
(export
 (define (make-machine)
   (define register-table
     (append (map (lambda (symbol) (cons symbol (make-register symbol 1)))
		  '(a b c d e f h l))
	     (map (lambda (symbol) (cons symbol (make-register symbol 2)))
		  '(pc sp))))

   (define internal-ram (make-internal-ram))

   (define (get-register symbol)
     "Return the register associated with symbol."
     (or (alist-ref register-table symbol)
	 (error "GET-REGISTER: Could not find register ~S" symbol)))

   (define pc-register (get-register 'pc))
   (define get-pc [pc-register :get-contents])
   (define set-pc! [pc-register :set-contents!])
   (define (advance-pc! (amount 1))
     [set-pc! (+ amount [get-pc])])

   (bundle 'machine? (make-machine)
	   get-register
	   advance-pc!)))

(uninstall-syntax!)
(defpackage-form :machine)
#+nil
(DEFPACKAGE #:MACHINE
  (:USE #:BINARY #:EXAMPLE #:RAM #:SCHEMEISH.SCHEMEISH)
  (:EXPORT #:MACHINE?
	   #:MAKE-MACHINE
	   #:MAKE-REGISTER
	   #:MAKE-ROM
	   #:R-CODE->GETTER
	   #:R-CODE->SETTER
	   #:REGISTER-GETTER
	   #:REGISTER-SETTER
	   #:REGISTER?
	   #:ROM?))
