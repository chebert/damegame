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

(uninstall-syntax!)
