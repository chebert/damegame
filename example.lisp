(in-package #:example)

(for-macros
  (defvar *examples-table* (make-hash-table)))

(define (get-examples symbol)
  (gethash symbol *examples-table*))
(define (set-examples! symbol examples)
  (setf (gethash symbol *examples-table*) examples))

(define (remove-examples! symbol)
  (remhash symbol *examples-table*))

(define-struct example
    (value-promise expected-promise
     value-form expected-form))

(define (example-value example)
  (force (example-value-promise example)))
(define (example-expected-value example)
  (force (example-expected-promise example)))

(define (example-spec->make-example-form example-spec)
  (cond
    ((list? example-spec)
     (let ((length (length example-spec)))
       (cond
	 ((= 1 length)
	  (let ((test-form (first example-spec)))
	    `(make-example (delay ,test-form)
			   (delay t)
			   ',test-form
			   t)))
	 ((= 2 length)
	  (let ((value-form (first example-spec))
		(expected-form (second example-spec)))
	    `(make-example (delay ,value-form)
			   (delay ,expected-form)
			   ',value-form
			   ',expected-form)))
	 (t
	  (error "Expected example-spec of the form (TEST) or (VALUE EXPECTED).")))))
    (t (error "Expected example-spec of the form (TEST) or (VALUE EXPECTED)."))))

(export
 (defmacro define-examples (name &body example-specs)
   `(for-macros
      (set-examples! ',name (list ,@(map #'example-spec->make-example-form example-specs)))
      ',name)))

(export
 (defmacro undefine-examples (name &rest ignored)
   (declare (ignore ignored))
   `(for-macros
      (remove-examples! ,name))))

(define (check-example symbol example)
  (let ((value (example-value example))
	(expected (example-expected-value example)))
    (unless (equal? value expected)
      (error "CHECK-EXAMPLE ~S: Expected ~S but got ~S." symbol expected value))))

(export
 (define (check-examples symbol)
   (let ((examples (get-examples symbol)))
     (for-each (lcurry #'check-example symbol)
	       examples)
     (length examples))))

(export
 (define (check-all-examples)
   (maphash (lambda (symbol examples)
	      (declare (ignore examples))
	      (check-examples symbol))
	    *examples-table*)
   (hash-table-count *examples-table*)))

(define-examples check-examples
  ((+ 1 2 3) 6))


(defpackage-form :example)



