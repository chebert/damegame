(unless (find-package :operations)
  (make-package :operations :use '(:schemeish.schemeish)))

(in-package :operations)

(extend-package :operations
		(document "Provides arithmetic, logical, and rotate/shift ops which return (values result . flags)")
		(package-use :example :binary))

(define (op-result result (:zero?) (:carry?) (:half-carry?) (:subtraction?))
  "(values result zero? carry? half-carry? subtraction?)"
  (values result carry? half-carry? subtraction? zero?))
(define (test-op-result (:zero?) (:carry?) (:half-carry?) (:subtraction?))
  "(values zero? carry? half-carry? subtraction?)"
  (values carry? half-carry? subtraction? zero?))

;; Arithmetic operations

(export
 (define (sub-op a b (carry 0))
   "Arithmetic-op, returns op-result"
   (let ((result (u8 (- a b carry))))
     (op-result result
		:carry? (borrow8? a b carry)
		:half-carry? (half-borrow8? a b carry)
		:subtraction? t
		:zero? (zero? result)))))

(export
 (define (add8-op a b (carry 0))
   "Arithmetic-op, returns op-result"
   (let ((result (u8 (+ a b carry))))
     (op-result result
		:carry? (carry8? a b carry)
		:half-carry? (half-carry8? a b carry)
		:subtraction? nil
		:zero? (zero? result)))))

(define-examples add8-op
  ((multiple-value-list (add8-op 126 1 1))
   '(128 NIL T NIL NIL))
  ((multiple-value-list (add8-op 255 1 0))
   '(0 T T NIL T)))

(export
 (define (add16-op sp e)
   "Arithmetic-op, returns op-result"
   (op-result (value->u16 (+ sp e))
	      :carry? (carry16? sp e)
	      :half-carry? (half-carry16? sp e)
	      :subtraction? nil
	      :zero? nil)))

(define-examples add16-op
  ((multiple-value-list (add16-op #xffff 1))
   '(0 T T NIL NIL)))

;; Logical Operations

(define ((logical-op op half-carry?) a b)
  "Arithmetic-op, applies [op a b], and returns op-result."
  (let ((result [op a b]))
    (op-result result
	       :carry? nil
	       :half-carry? half-carry?
	       :subtraction? nil
	       :zero? (zero? result))))

;; For whatever reason and sets the half-carry.
(export (define and-op (logical-op 'logand t)))
(export (define or-op  (logical-op 'logior nil)))
(export (define xor-op (logical-op 'logxor nil)))

(define-examples and-op
  ((multiple-value-list (and-op #b101 #b010))
   '(0 NIL T NIL T))
  ((multiple-value-list (and-op #b011 #b010))
   '(2 NIL T NIL NIL)))

;; Rotate/Shift Operations
(define (bit->boolean bit) (= bit 1))

;; RLA, RRA, etc. reset the zero? flag
(define (rotate-a-op-result result bit)
  "Returns op-result for the result of rotating A. E.g. using RLA, RRA, etc."
  (op-result result
	     :carry? (bit->boolean bit)
	     :half-carry? nil
	     :subtraction? nil
	     :zero? nil))

;; RL r, RR r, etc. set the zero? flag based on the result
(define (rotate-op-result result bit)
  "Returns op-result for a shifted/rotated value."
  (op-result result
	     :carry? (bit->boolean bit)
	     :half-carry? nil
	     :subtraction? nil
	     :zero? (zero? result)))

(export (define rla-op (compose 'rotate-a-op-result 'rotate-left)))
(export (define rra-op (compose 'rotate-a-op-result 'rotate-right)))
(export (define (rlca-op byte) (rla-op byte (bit-value byte 7))))
(export (define (rrca-op byte) (rra-op byte (bit-value byte 0))))

(export (define rl-op (compose 'rotate-op-result 'rotate-left)))
(export (define rr-op (compose 'rotate-op-result 'rotate-right)))
(export (define (rlc-op byte) (rl-op byte (bit-value byte 7))))
(export (define (rrc-op byte) (rr-op byte (bit-value byte 0))))

(export (define (sla-op byte) [(compose 'rotate-op-result 'rotate-left)  byte 0]))
(export (define (sra-op byte) [(compose 'rotate-op-result 'rotate-right) byte (bit-value byte 7)]))
(export (define (srl-op byte) [(compose 'rotate-op-result 'rotate-right) byte 0]))

(export
 (define (swap-op byte)
   (let ((result (swap-nibbles byte)))
     (op-result result
		:zero? (zero? result)
		:carry? nil
		:half-carry? nil
		:subtraction? nil))))
