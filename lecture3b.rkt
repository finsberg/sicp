#lang racket

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (deriv expr var)
  (cond ((constant? expr var) 0)
        ((same-variable? expr var) 1)
        ((sum? expr)
         (make-sum (deriv (a1 expr) var)
                   (deriv (a2 expr) var)))
        ((product? expr)
         (make-sum
          (make-product (m1 expr)
                        (deriv (m2 expr) var))
          (make-product (deriv (m1 expr) var)
                        (m2 expr))))))


(define (constant? expr var)
  (and (atom? expr)
       (not (eq? expr var))))


(define (same-variable? expr var)
  (and (atom? expr)
       (eq? expr var)))


(define (sum? expr)
  (and (not (atom? expr))
       (eq? (car expr) '+)))

#|
(define (make-sum a1 a2)
  (list '+ a1 a2))
|#

(define a1 cadr)
(define a2 caddr)

(define (product? expr)
  (and (not (atom? expr))
       (eq? (car expr) '*)))

#|
(define (make-product m1 m2)
  (list '* m1 m2))
|#

(define m1 cadr)
(define m2 caddr)


; Change represetentation
(define (make-sum a1 a2)
  (cond ((and (number? a1)
              (number? a2))
         (+ a1 a2))
        ((and (number? a1) (= a1 0))
         a2)
        ((and (number? a2) (= a2 0))
         a1)
        (else (list '+ a1 a2))))


; Change represetentation
(define (make-product m1 m2)
  (cond ((and (number? m1)
              (number? m2))
         (* m1 m2))
        ((and (number? m1) (= m1 0))
         0)
        ((and (number? m2) (= m2 0))
         0)
        ((and (number? m1) (= m1 1))
         m2)
        ((and (number? m2) (= m2 1))
         m1)
        (else (list '* m1 m2))))

; define axx + bx + c
(define foo
    '(+ (* a (* x x))
        (+ (* b x)
           c)))

; compute the derivative
(deriv foo 'x)
(deriv foo 'a)