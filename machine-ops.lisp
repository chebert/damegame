(unless (find-package :machine-ops)
  (make-package :machine-ops :use '(:schemeish.schemeish)))

(in-package :machine-ops)

;; TODO: Rename MACHINE-OPS to MACHINE-ENCODING
;; TODO: Move stack effects to their own package

(extend-package :machine-ops
		(package-use :example :assembly))


(export
 (define machine-op? (make-bundle-predicate :machine-op)))

(define (make-stack-effect parameters results)
  (list parameters results))
(define (make-stack-effects  parameters results)
  "Return a list of (stack-effect ...)."
  (list (make-stack-effect parameters results)))

(export
 (define (stack-effect-parameters stack-effect)
   "Returns the parameters of a stack-effect."
   (first stack-effect)))
(export
 (define (stack-effect-results stack-effect)
   "Returns the results of a stack-effect."
   (second stack-effect)))

(export
 (define (stack-effects-of-size? stack-effects num-parameters num-results)
   "Returns true if all of stack-effects have the provided number of parameters and results."
   (for-all (lambda (effect)
	      (and (= (length (stack-effect-parameters effect)) num-parameters)
		   (= (length (stack-effect-results effect)) num-results)))
	    stack-effects)))


(export
 (define (machine-op-stack-effects machine-op)
   "Return a list of all possible  (((consumed-argument ...) (returned-value ...)))."
   [[machine-op :stack-effects]]))

(define (sequence-stack-effect op1-effect op2-effect)
  "Returns a list of stack-effects when op1 is performed in sequence before op2"
  (let ((results (stack-effect-results op1-effect))
	(parameters (stack-effect-parameters op2-effect)))
    (let ((num-results (length results))
	  (num-parameters (length parameters)))
      (cond
	((< num-results num-parameters)
	 ;; ( [a..] -- [n..] ) sequenced with
	 ;; ( [m..] [n..] -- [r..] ) results in
	 ;; ( [m..] [a..] -- [r..] )
	 (list (make-stack-effect (append (take (stack-effect-parameters op2-effect) (- num-parameters num-results))
					  (stack-effect-parameters op1-effect))
				  (stack-effect-results op2-effect))))
	((= num-results num-parameters)
	 ;; ( [a..] -- [n..] ) sequenced with
	 ;; ( [n..] -- [r..] ) results in
	 ;; ( [a..] -- [r..] )
	 (list (make-stack-effect (stack-effect-parameters op1-effect)
				  (stack-effect-results op2-effect))))
	((> num-results num-parameters)
	 ;; ( [a..] -- [m..] [n..] ) sequenced with
	 ;; ( [n..] -- [r..] ) results in
	 ;; ( [a..] -- [m..] [r..] )
	 (list (make-stack-effect (stack-effect-parameters op1-effect)
				  (append (take (stack-effect-results op1-effect) (- num-results num-parameters))
					  (stack-effect-results op2-effect)))))))))


(define-examples sequence-stack-effect
  ((sequence-stack-effect (make-stack-effect '(a0 a1) '(n0 n1))
			  (make-stack-effect '(m0 m1 n0 n1) '(r0 r1)))
   '(((M0 M1 A0 A1) (R0 R1))))

  ((sequence-stack-effect (make-stack-effect '(a0 a1) '(n0 n1))
			  (make-stack-effect '(n0 n1) '(r0 r1)))
   '(((A0 A1) (R0 R1))))

  ((sequence-stack-effect (make-stack-effect '(a0 a1) '(m0 m1 n0 n1))
			  (make-stack-effect '(n0 n1) '(r0 r1)))
   '(((A0 A1) (M0 M1 R0 R1)))))

(define (sequence-stack-effects machine-op1-effects machine-op2-effects)
  "Return the stack effects of performing machine-op1 before performing machine-op2."
  (remove-duplicates
   (append-map (lambda (op1-effect)
		 (append-map (lambda (op2-effect) (sequence-stack-effect op1-effect op2-effect))
			     machine-op2-effects))
	       machine-op1-effects)
   :test #'equal?))

(define-examples sequence-stack-effects
  ((sequence-stack-effects (list (make-stack-effect '(a0 a1) '(n0 n1))
				 (make-stack-effect '(a0 a1) '(m0 m1 n0 n1)))
			   (list (make-stack-effect '(m0 m1 n0 n1) '(r0 r1))
				 (make-stack-effect '(n0 n1) '(r0 r1))))
   '(((M0 M1 A0 A1) (R0 R1))
     ((A0 A1) (R0 R1))
     ((A0 A1) (M0 M1 R0 R1)))))

(export
 (define (machine-op->list machine-op)
   "Return a list representation of machine-op."
   [[machine-op :as-list]]))

(export
 (define (nop)
   "No operation."
   (define (stack-effects) (make-stack-effects () ()))
   (define (as-list)
     (list `(nop)))
   (bundle 'machine-op? ()
	   stack-effects
	   as-list)))

(define-examples nop
  ((machine-op-stack-effects (nop)) '((() ())))
  ((machine-op->list (nop)) '((nop))))


(export
 (define (constant value)
   "Push a constant value onto the stack."
   (define (stack-effects) (make-stack-effects () (list value)))
   (define (as-list) (list `(constant ,value)))
   (bundle 'machine-op? ()
	   stack-effects
	   as-list)))

(define-examples constant
  ((machine-op-stack-effects (constant 3)) '((NIL (3))))
  ((machine-op->list (constant 3)) '((constant 3))))


(export
 (define (seq . machine-ops)
   "Performs machine-ops in order."

   (define (seq* machine-ops)
     (define (stack-effects)
       (foldl (lambda (machine-op effects)
		(sequence-stack-effects effects (machine-op-stack-effects machine-op)))
	      (machine-op-stack-effects (first machine-ops))
	      (rest machine-ops)))
     (define (as-list) (append-map 'machine-op->list machine-ops))
     (bundle 'machine-op? ()
	     stack-effects
	     as-list))

   (cond
     ((null? machine-ops) (nop))
     ((null? (cdr machine-ops)) (first machine-ops))
     (t (seq* machine-ops)))))


(define-examples seq
  ((machine-op-stack-effects (seq (constant 1) (seq (constant 2) (constant 3))))
   '((NIL (1 2 3))))
  ((machine-op->list (seq (constant 1) (seq (nop) (constant 2))))
   '((CONSTANT 1) (NOP) (CONSTANT 2))))



(export
 (define (alt true-op false-op)
   "Pops test off of stack and condition off of stack and performs true-op if true, 
else false-op."
   (define (stack-effects)
     (map (lambda (effect)
	    (make-stack-effect
	     (cons 'test? (stack-effect-parameters effect))
	     (stack-effect-results effect)))
	  (append
	   (machine-op-stack-effects true-op)
	   (machine-op-stack-effects false-op))))
   (define (as-list)
     (list `(alt ,(machine-op->list true-op)
		 ,(machine-op->list false-op))))
   (bundle 'machine-op? ()
	   stack-effects
	   as-list)))

(define-examples alt
  ((machine-op-stack-effects (alt (seq (constant 1))
				  (seq (constant 2)
				       (constant 3))))
   '(((TEST?) (1)) ((TEST?) (2 3))))
  ((machine-op->list (alt (seq (constant 1))
			  (seq (constant 2)
			       (constant 3))))
   '((ALT ((CONSTANT 1)) ((CONSTANT 2) (CONSTANT 3))))))


(export
 (define (reg register-name)
   "Push contents of register-name onto the stack."
   (define (stack-effects)
     (if (register16? register-name)
	 (make-stack-effects () '(u16))
	 (make-stack-effects () '(u8))))
   (define (as-list) (list `(reg ,register-name)))
   (bundle 'machine-op? ()
	   stack-effects
	   as-list)))

(export
 (define (assign-reg register-name)
   "Pops top of stack into register."
   (define (stack-effects)
     (if (register16? register-name)
	 (make-stack-effects '(u16) ())
	 (make-stack-effects '(u8) ())))
   (define (as-list) (list `(assign-reg ,register-name)))
   (bundle 'machine-op? ()
	   stack-effects
	   as-list)))

(export
 (define (flag flag-name)
   "Push boolean value of flag onto stack."
   (define (stack-effects) (make-stack-effects () '(boolean)))
   (define (as-list) (list `(flag ,flag-name)))
   (bundle 'machine-op? ()
	   stack-effects
	   as-list)))

(export
 (define (assign-flag flag-name)
   "Pops boolean off stack and stores into flag."
   (define (stack-effects) (make-stack-effects '(boolean) ()))
   (define (as-list) (list `(assign-flag ,flag-name)))
   (bundle 'machine-op? ()
	   stack-effects
	   as-list)))

(export
 (define (fetch)
   "Pops address off of stack and pushes value at address."
   (define (stack-effects) (make-stack-effects '(address) '(u8)))
   (define (as-list) (list `(fetch)))
   (bundle 'machine-op? ()
	   stack-effects
	   as-list)))

(export
 (define (store)
   "Pops address and value off of stack and stores value at address."
   (define (stack-effects) (make-stack-effects '(u8 address) ()))
   (define (as-list) (list '(store)))
   (bundle 'machine-op? ()
	   stack-effects
	   as-list)))

(export
 (define (advance-pc count)
   "Increments PC."
   (define (stack-effects) (make-stack-effects () ()))
   (define (as-list) (list `(advance-pc ,count)))
   (bundle 'machine-op? ()
	   stack-effects
	   as-list)))
(export
 (define (goto)
   "Pops address off of stack and stores in PC."
   (define (stack-effects) (make-stack-effects '(address) ()))
   (define (as-list) (list `(goto)))
   (bundle 'machine-op? ()
	   stack-effects
	   as-list)))
(export
 (define (branch)
   "Pops address and condition off of stack and jumps if the condition is truthy."
   (define (stack-effects) (make-stack-effects '(test? address) ()))
   (define (as-list) (list `(branch)))
   (bundle 'machine-op? ()
	   stack-effects
	   as-list)))

(export
 (define (perform num-arguments op (num-results 1))
   "Pops num-arguments off of the stack, applies lisp procedure op to them, and pushes num-results values returned by op onto the stack."
   (define (names base n)
     (map (lambda (i) (intern (string-append base (number->string i))))
	  (range n)))

   (define (stack-effects)
     (let ((args (names "ARG" num-arguments))
	   (results (names "RESULT" num-results)))
       (make-stack-effects args results)))
   
   (define (as-list) (list `(perform ,num-arguments ,op ,num-results)))

   (bundle 'machine-op? ()
	   stack-effects
	   as-list)))

(export
 (define (enable-interrupts)
   "Enables interrupts."
   (define (stack-effects) (make-stack-effects () ()))
   (define (as-list) (list `(enable-interrupts)))
   (bundle 'machine-op? ()
	   stack-effects
	   as-list)))

(define-examples machine-op->list
  ((machine-op->list
    (seq (flag 'zero?)
	 (alt (seq (reg 'sp) (perform 1 '1+ 1) (assign-reg 'sp))
	      (nop))))
   '((FLAG ZERO?)
     (ALT
      ((REG SP) (PERFORM 1 1+ 1) (ASSIGN-REG SP))
      ((NOP))))))
