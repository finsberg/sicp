#lang racket

; square
(define (sq x) (* x x))

; sum of squares
(define (sos x y)
  (+ (sq x) (sq y)))

; plus iteration
(define (plus1 x y)
  (if (= x 0)
      y
      (plus1 (sub1 x) (add1 y))))

; plus recursion
(define (plus2 x y)
  (if (= x 0)
      y
      (add1 (plus2 (sub1 x) (y)))))

; fibonacci numbers
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

; tower of hanoi
(define (move n from to spare)
  (cond ((= n 0) #t)
        (else
         (move (sub1 n) from spare to)
         (printf "move ~a to ~s, " from to)
         (move (sub1 n) spare to from))))