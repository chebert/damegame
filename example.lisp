(in-package #:example)

;; TODO

(for-macros
  (defvar *examples-table* (make-hash-table)
    "Hash table of symbol -> (example ...)"))

(define (get-examples symbol)
  "Returns examples associated with symbol."
  (gethash symbol *examples-table*))
(define (set-examples! symbol examples)
  "Sets associates examples wiith symbol."
  (setf (gethash symbol *examples-table*) examples))
(define (remove-examples! symbol)
  "Removes examples associated with symbol."
  (remhash symbol *examples-table*))

(define-struct example
    (value-promise expected-promise
     value-form expected-form))

(define (example-value example)
  "Forces the value-promise in example."
  (force (example-value-promise example)))
(define (example-expected-value example)
  "Forces the expected-promise in example."
  (force (example-expected-promise example)))

(define (example-spec->make-example-form example-spec)
  (cond
    ((list? example-spec)
     (let ((length (length example-spec)))
       (cond
	 ((= 1 length)
	  ;; example-spec := (TEST-FORM)
	  (let ((test-form (first example-spec)))
	    `(make-example (delay ,test-form)
			   (delay t)
			   ',test-form
			   t)))
	 ((= 2 length)
	  ;; example-spec := (VALUE-FORM EXPECTED-FORM)
	  (let ((value-form (first example-spec))
		(expected-form (second example-spec)))
	    `(make-example (delay ,value-form)
			   (delay ,expected-form)
			   ',value-form
			   ',expected-form)))
	 (t (error "Expected example-spec of the form (TEST) or (VALUE EXPECTED). Got ~S" example-spec)))))
    (t (error "Expected example-spec of the form (TEST) or (VALUE EXPECTED). Got ~S" example-spec))))


(define (check-example symbol example)
  (let ((value (example-value example))
	(expected (example-expected-value example)))
    (unless (equal? value expected)
      (error "CHECK-EXAMPLE ~S: Expected ~S but got ~S." symbol expected value))))

(export
 (define (check-examples symbol)
   "Checks only examples associated with symbol."
   (let ((examples (get-examples symbol)))
     (for-each (lcurry #'check-example symbol)
	       examples)
     (length examples))))

(export
 (define (check-all-examples)
   "Checks all currently stored examples."
   (maphash (lambda (symbol examples)
	      (declare (ignore examples))
	      (check-examples symbol))
	    *examples-table*)
   (hash-table-count *examples-table*)))

(export
 (defmacro define-examples (name &body example-specs)
   "Associate example-specs with name.

example-spec := (VALUE-FORM) | (VALUE-FORM EXPECTED-FORM)
If example-spec is a only a value-form, it is compared against t.
Otherwise value is compared using EQUAL? against expected."
   `(for-macros
      (set-examples! ',name (list ,@(map #'example-spec->make-example-form example-specs)))
      ',name)))

(export
 (defmacro undefine-examples (name &rest ignored)
   "Dissociates examples from name."
   (declare (ignore ignored))
   `(for-macros
      (remove-examples! ',name))))

(define-examples check-examples
  ((+ 1 2 3) 6)
  ((not (ignore-errors (error "Throws an error")))))
