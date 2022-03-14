#lang racket

(define list-1-to-4-cons
  (cons 1
        (cons 2
              (cons 3
                    (cons 4 null)))))

(define list-1-to-4 (list 1 2 3 4))

(define (scale-list s l)
  (if (null? l)
      null
      (cons (* (car l) s)
            (scale-list s (cdr l)))))
; (scale-list 10 list-1-to-4)

; We can define a higher order procedue (there is allreade a proc map
(define (my_map p l)
  (if (null? l)
      null
      (cons (p (car l))
            (my_map p (cdr l)))))

(define (scale-10 x) (* x 10))
;(my_map scale-10 list-1-to-4)

(define (scale-list-map s l)
  (map (lambda(item) (* item  s)) l))
; (scale-list-map 10 list-1-to-4)