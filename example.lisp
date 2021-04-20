(in-package #:example)

(for-macros
  (defvar *examples* (make-hash-table)))

(define (set-example! symbol example)
  (setf (gethash symbol *examples*) example))

(define (remove-example! symbol)
  (remhash symbol *examples*))

(define (make-example value-promise expected-promise)
  (cons value-promise expected-promise))
(define (example-value example)
  (force (car example)))
(define (example-expected-value example)
  (force (cdr example)))

(defmacro define-example (name expected-form &body value-body)
  `(for-macros
     (set-example! ',name (make-example (delay ,@value-body)
					(delay ,expected-form)))
     ',name))

(defmacro undefine-example (name &rest ignored)
  (declare (ignore ignored))
  `(for-macros
     (remove-example! ,name)))

(define (check-example name example)
  (let ((value (example-value example))
	(expected (example-expected-value example)))
    (unless (equal? value expected)
      (error "CHECK-EXAMPLE: ~S. Expected ~S. Got ~S" name expected value))))

(define (check-examples)
  (maphash #'check-example *examples*)
  (hash-table-count *examples*))

(define-example check-examples
    6
  (+ 1 2 3))
