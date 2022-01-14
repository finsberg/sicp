#lang racket
#| Sicp lecutre 1
Fixed point method for finding the square root
|#

(define (square x) (* x x))

(define (Abs x)
    (cond ((< x 0) (- x))
          ((= x 0) 0)
          ((> x 0) x)))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     0.001))

(define (try guess x)
    (if (good-enough? guess x)
        guess
        (try (improve guess x) x)))

(define (sqrt x) (try 1 x))

; This is the same as sqrt but it displays the values are floats
(define (isqrt x) (exact->inexact (sqrt x)))