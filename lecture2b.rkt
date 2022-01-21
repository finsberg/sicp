#lang racket

; Make rational numbers
(define (make-rat n d)
  (cons n d))
; We could simplify the fraction by dividing by the greatest common divisor


; Get numerator of rational number
(define (numer x)
  (car x))

; Get denominator of rational number
(define (denom x)
  (cdr x))

; Method for adding rational numbers
(define (+rat x y)
  (make-rat
   (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

; Method for multiplying rational numbers
(define (*rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))


#|
and = A + B = 1/2 + 1/4
|#
(define A (make-rat 1 2))
(define B (make-rat 1 4))
(define ans (+rat A B))


; Vectors in the place - constructor
(define (make-vector x y) (cons x y))

; x-coordinate - selector
(define (xcor p) (car p))

; y-coordinate - selector
(define (ycor p) (cdr p))

; Line segments between two points in the place
(define (make-seg p q) (cons p q))

; Start of line segment
(define (seg-start s) (car s))
; End of line segment
(define (seg-end s) (cdr s))

; Midpoint of a line segment
(define (mid-point s)
  (define (average x y)
    (/ (+ x y) 2))
  (let ((a (seg-start s))
        (b (seg-end s)))
    (make-vector (average (xcor a) (xcor b))
                 (average (ycor a) (ycor b)))))
; Similarly we can also compute the length of a line segment

; Procedures for making pairs
(define (make-pair a b)
  (lambda(pick)
    (cond ((= pick 1) a)
          ((= pick 2) b))))

(define (fst pair) (pair 1))
(define (snd pair) (pair 2))
