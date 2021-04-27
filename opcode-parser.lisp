(in-package #:opcode-parser)

(install-syntax!)

(extend-package
 :opcode-parser
 (document "Provides tools for parsing register-names from opcodes.")
 (package-use :schemeish.schemeish :machine :opcode-compiler :binary))


(export
 (define (r-code->register-name code)
   "Given an an r-code extracted from an opcode, return the register-name or '(hl)."
   (cond
     ((= code #b111) :a)
     ((= code #b000) :b)
     ((= code #b001) :c)
     ((= code #b010) :d)
     ((= code #b011) :e)
     ((= code #b100) :h)
     ((= code #b101) :l)
     ((= code #b110) '(:hl))
     (t (error "R-CODE->REGISTER-NAME: don't recognize code ~S" code)))))
(export
 (define (r-code->getter machine code)
   "Return a compiled (lambda () byte) that fetches the byte associated with the given r-code."
   (let ((name (r-code->register-name code)))
     (if (equal? name '(:hl))
	 (let ((get-hl (register-getter machine :hl))
	       (get-byte [machine :get-byte]))
	   (lambda () [get-byte [get-hl]]))
	 (register-getter machine name)))))
(export
 (define (r-code->setter machine code)
   "Return a compiled (lambda (byte)) that sets the byte associated with the given r-code."
   (let ((name (r-code->register-name code)))
     (if (equal? name '(:hl))
	 (let ((get-hl (register-getter machine :hl))
	       (set-byte [machine :set-byte!]))
	   (lambda () [set-byte [get-hl]]))
	 (register-setter machine name)))))


(export
 (define (ss-code->register-name ss-code)
   "Return the register or register-pair name associated with ss-code extracted from an opcode."
   (cond
     ((= ss-code #b00) :bc)
     ((= ss-code #b01) :de)
     ((= ss-code #b10) :hl)
     ((= ss-code #b11) :sp)
     (t (error "Unrecognized SS-CODE: ~S" ss-code)))))

(export
 (define (extract-ss-register-name opcode)
   (ss-code->register-name (bit-extract opcode 4 2))))

(export
 (define (dd-code->register-name code)
   (cond
     ((= code #b00) :bc)
     ((= code #b01) :de)
     ((= code #b10) :hl)
     ((= code #b11) :sp)
     (t (error "Unrecognized DD register-pair code ~S" code)))))
(export
 (define (qq-code->register-name code)
   (cond
     ((= code #b00) :bc)
     ((= code #b01) :de)
     ((= code #b10) :hl)
     ((= code #b11) :af)
     (t (error "Unrecognized QQ register-pair code ~S" code)))))

(export
 (define (extract-dd-register-name opcode)
   (dd-code->register-name (bit-extract opcode 4 2))))
(export
 (define (extract-qq-register-name opcode)
   (qq-code->register-name (bit-extract opcode 4 2))))


(uninstall-syntax!)
