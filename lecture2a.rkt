#lang racket

(define (sum-int a b)
  (if (> a b)
      0
      (+ a (sum-int (add1 a) b))))


(define (square a) (* a a))

(define (sum-sq a b)
  (if (> a b)
      0
      (+ (square a) (sum-sq (add1 a) b))))

; Leibniz approximation of pi/8
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2))) (pi-sum (+ a 4) b))))

; Recursive implementation of sum
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))


(define (sum-int2 a b)
  (define (identity a) (a))
  (sum identity a add1 b))

(define (sum-sq2 a b) (sum square a add1 b))

(define (pi-sum2 a b)
  (sum (lambda(i) (/ 1 (* i (+ i 2)))) a (lambda(i) (+ 4 i)) b))

; Iterative implementation of sum
(define (sum-iter term a next b)
  (define (iter j ans)
    (if (> j b)
        ans
        (iter (next j)
              (+ (term j) ans))))
  (iter a 0))


; Fixed point method for sqrt(x)

(define (average x y) (/ (+ x y) 2))
(define (abs x)
  (if (> x 0)
      x
      (- x)))

(define (sqrt1 x)
  (fixed-point
   (lambda(y) (average (/ x y) y))
   1))

; Higher order fixed point method
(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enuf? u v)
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter start (f start)))

; y -> x/y is also a fixed point but it ocscilates but we can avarage-damp it
(define (sqrt2 x)
  (fixed-point
  (average-damp (lambda(y) (/ x y)))
  1))

(define average-damp
  (lambda(f)
  (lambda(x) (average x (f x)))))

; Newtons method solve (f(y) = 0)

(define (sqrt3 x)
  (newton (lambda(y) (-x (square y)))
          1))