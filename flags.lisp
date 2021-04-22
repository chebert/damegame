(in-package #:flags)

(install-syntax!)

(defparameter *carry-bit-index* 4)
(defparameter *half-carry-bit-index* 5)
(defparameter *subtraction-bit-index* 6)
(defparameter *zero-bit-index* 7)

(defparameter *carry-mask* (bit-set 0 *carry-bit-index*))
(defparameter *half-carry-mask* (bit-set 0 *half-carry-bit-index*))
(defparameter *subtraction-mask* (bit-set 0 *subtraction-bit-index*))
(defparameter *zero-mask* (bit-set 0 *zero-bit-index*))

(export
 (define (flags zero? subtraction? half-carry? carry?)
   "Return the value of the flags register with the given flags set/unset."
   (+ (if zero? *zero-mask* 0)
      (if subtraction? *subtraction-mask* 0)
      (if half-carry? *half-carry-mask* 0)
      (if carry? *carry-mask* 0))))

(export
 (define (carry-set? flags)
   (bit-set? flags *carry-bit-index*)))
(export
 (define (zero-set? flags)
   (bit-set? flags *zero-bit-index*)))
(export
 (define (subraction-set? flags)
   (bit-set? flags *subtraction-bit-index*)))
(export
 (define (half-carry-set? flags)
   (bit-set? flags *half-carry-bit-index*)))

(define-examples flags
  ((flags t nil t nil) #b10100000)
  ((flags t t t t) #b11110000)
  ((flags nil nil nil nil) #b00000000))

(export
 (define (carry-bit flags)
   "Return the carry bit value in flags."
   (bit-value flags *carry-bit-index*)))

(define-examples carry-bit
  ((carry-bit (flags nil nil nil t)) 1)
  ((carry-bit (flags nil nil nil nil)) 0))

(export
 (define (flag-mask flag-name)
   "Return the flag-mask associated with the given flag-name."
   (cond
     ((eq? flag-name :zero?) *zero-mask*)
     ((eq? flag-name :subtraction?) *subtraction-mask*)
     ((eq? flag-name :half-carry?) *half-carry-mask*)
     ((eq? flag-name :carry?) *carry-mask*)
     (t (error "FLAG-MASK: Unknown flag-name ~S" flag-name)))))

(export
 (define (flags->list flags)
   "Return a list of flag-names set in the flags register."
   (remove nil (map (lambda (flag-name)
		      (and (not (zero? (logand (flag-mask flag-name) flags))) flag-name))
		    '(:zero? :subtraction? :half-carry? :carry?)))))

(uninstall-syntax!)

(defpackage-form :flags)
