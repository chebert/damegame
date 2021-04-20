(in-package #:geometry)

(define-struct v2
    (x y)
    :transparent)

(define make-point #'make-v2)
(define point-x #'v2-x)
(define point-y #'v2-y)
(define point? #'v2?)

(define make-offset #'make-v2)
(define offset-x #'v2-x)
(define offset-y #'v2-y)
(define offset? #'v2?)

(define (v2-zero) (make-v2 0 0))

(define (v2+-bin a b)
  (make-v2 (+ (v2-x a) (v2-x b))
	   (+ (v2-y a) (v2-y b))))

(define (v2+ . vs)
  (foldl #'v2+-bin (v2-zero) vs))

(define-example v2+-0
    (v2-zero)
  (v2+))

(define-example v2+-1
    (make-v2 1 2)
  (v2+ (make-v2 1 2)))

(define-example v2+
    (make-v2 9 12)
  (v2+ (make-v2 1 2)
       (make-v2 3 4)
       (make-v2 5 6)))

(define (v2--bin a b)
  (make-v2 (- (v2-x a) (v2-x b))
	   (- (v2-y a) (v2-y b))))

(define (v2- v . vs)
  (foldl (swap-args #'v2--bin) v vs))

(define-example v2-
    (make-v2 (- 1 2 3) (- 1 2 3))
  (v2- (make-v2 1 1)
       (make-v2 2 2)
       (make-v2 3 3)))

(define-struct rect
    (x y width height)
    :transparent)

(define (in-range? value min length)
  "True if value is in the range [min, min+length).
Lower bound inclusive, upper bound exclusive."
  (<= min value (1- (+ min length))))

(define-example in-range?
    '(t nil t)
  (list
   (in-range? 3 3 2)
   (in-range? 5 3 2)
   (in-range? 4 3 2)))

(define (point-in-rect? point rect)
  "True if the given v2 point is inside of the rect top-left inclusive, bottom-right exclusive."
  (and (in-range? (point-x point) (rect-x rect) (rect-width rect))
       (in-range? (point-y point) (rect-y rect) (rect-height rect))))

(define-example point-in-rect?
    '(t nil t)
  (list
   (point-in-rect? (make-point 3 4)
		   (make-rect 3 4 2 2))
   (not (point-in-rect? (make-point 5 6)
			(make-rect 3 4 2 2)))
   (point-in-rect? (make-point 4 5)
		   (make-rect 3 4 2 2))))
