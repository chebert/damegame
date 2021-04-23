(in-package #:opcode-compiler)

(install-syntax!)

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

(define (find-compiler opcode extended?)
  "Finds the appropriate compiler that matches opcode."
  (let ((table (get-compiler-table extended?)))
    (let ((key (hash-find-keyf table (rcurry #'opcode-matches-template? opcode))))
      (if key
	  (hash-ref table key)
	  (error "FIND-COMPILER: No compiler found that matches ~aopcode ~S"
		 (if extended? "extended " "") opcode)))))

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
	(let ((extended-opcode (aref memory (1+ address)))
	      (immediate8 (safe-vector-ref memory (+ 2 address) 0)))
	  (let ((immediate16 (u16 (safe-vector-ref memory (+ 3 address) 0) immediate8)))
	    [(find-compiler opcode t) machine extended-opcode immediate8 immediate16])))
       (t (let ((immediate8 (safe-vector-ref memory (1+ address) 0)))
	    (let ((immediate16 (u16 (safe-vector-ref memory (+ 2 address) 0)
				    immediate8)))
	      [(find-compiler opcode nil) machine opcode immediate8 immediate16])))))))


;; Construct full compilers from simple compilers.
(export
 (define ignore-immediates "Returns a compiler which ignores immediate arguments." (ignore-args 2 3)))
(export
 (define ignore-immediate8 "Returns a compiler which ignores the immediate8 argument." (ignore-args 2)))
(export
 (define ignore-immediate16 "Returns a compiler which ignores the immediate16 argument." (ignore-args 3)))
(export
 (define just-machine "Returns a compiler that ignores all arguments except machine." (ignore-args 1 2 3)))
(export
 (define just-immediate8 "Returns a compiler that ignores all arguments except machine and immediate8." (ignore-args 1 3)))
(export
 (define just-immediate16 "Returns a compiler that ignores all arguments except machine and immediate16." (ignore-args 1 2)))



(uninstall-syntax!)
