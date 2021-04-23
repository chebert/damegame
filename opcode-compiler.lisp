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

(define (vector-ref vector index)
  (aref vector index))
(define (vector-set! vector index value)
  (setf (aref vector index) value))

(define (safe-vector-ref vector index out-of-bounds-result)
  (if (>= index (length vector))
      out-of-bounds-result
      (aref vector index)))


(define (remove-indices indices list)
  "Remove all 0-based indices from list."
  (nreverse (foldl (lambda (arg index result)
		     (if (member index indices)
			 result
			 (cons arg result)))
		   ()
		   list
		   (range (length list)))))

(define-examples remove-indices
  ((remove-indices '(1 3) '(a b c d e f)) '(a c e f)))

(define ((ignore-args . indices) f)
  "Return a function, which when applied to a function F ignores positional arguments matching the 0-based indices."
  (lambda args
    (apply f (remove-indices indices args))))

(define-examples ignore-args
  ([[(ignore-args 1 4) (lambda (a c d) (list a c d))]
    :a :b :c :d :e]
   '(:A :C :D)))




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
(defpackage-form :machine)
