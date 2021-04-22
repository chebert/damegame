(in-package #:opcode-compiler)

(install-syntax!)

(use-package :binary)

;; TODO: move to schemeish
(define (hash-ref table key (failure-result))
  "Returns the value associated with key in the hash-table table or failure-result."
  (multiple-value-bind (value present?) (gethash key table)
    (if present?
	value
	failure-result)))
(define (hash-set! table key value)
  "Sets the value associated with key in hash-table table to value."
  (setf (gethash key table) value))
(define (hash-find-keyf table predicate (failure-result))
  "Returns the first key that satisfies [predicate key] in table."
  (loop for key being the hash-keys in table
	do (when [predicate key]
	     (return-from hash-find-keyf key)))
  failure-result)

(for-macros
  (defvar *compiler-table* (make-hash-table))
  (defvar *extended-compiler-table* (make-hash-table)))

(define (get-compiler-table extended?)
  "Returns the compiler-table. Extended? is true for extended-opcodes."
  (if extended?
      *extended-compiler-table*
      *compiler-table*))

(export
 (define (register-compiler! opcode-template extended? compiler)
   "Register the compiler to be used when an opcode matches opcode-template.
Extended? is true for extended-opcodes."
   (hash-set! (get-compiler-table extended?) opcode-template compiler)))

(export
 (define (find-compiler opcode extended?)
   "Finds the appropriate compiler that matches opcode."
   (let ((table (get-compiler-table extended?)))
     (let ((key (hash-find-keyf table (rcurry #'opcode-matches-template? opcode))))
       (if key
	   (hash-ref table key)
	   (error "FIND-COMPILER: No compiler found that matches ~aopcode ~S"
		  (if extended? "extended " "") opcode))))))

(export
 (define (opcode-matches-template? template opcode)
   "True if the opcode matches the template."
   (bit-mask-matches? template opcode)))

(export
 (define (compile-execution-proc machine memory address)
   "Compiles an execution-procedure given the instruction bytes in memory starting at address."
   (let ((opcode (aref memory address)))
     (cond
       ((= opcode #xCB)
	(let ((extended-opcode (aref memory (1+ address))))
	  [(find-compiler opcode t) machine extended-opcode memory address]))
       (t [(find-compiler opcode nil) machine opcode memory address])))))



;; TODO: move these into opcode-parser

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


(uninstall-syntax!)
(defpackage-form :machine)
