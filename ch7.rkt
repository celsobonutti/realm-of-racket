#lang racket

(define (my-map f l)
  (cond [(null? l) null]
        [else (cons (f (car l)) (my-map f (cdr l)))]))

(define (my-filter pred l)
  (cond [(null? l) null]
        [(pred (car l))
         (cons (car l) (my-filter pred (cdr l)))]
        [else (my-filter pred (cdr l))]))

(define (my-ormap pred lst)
  (cond [(null? lst) #f]
        [else (or (pred (car lst))
                  (my-ormap pred (cdr lst)))]))

(define (my-andmap pred lst)
  (cond [(null? lst) #t]
        [else (and (pred (car lst))
                   (my-andmap pred (cdr lst)))]))

(define (my-foldr f base lst)
  (cond [(empty? lst) base]
        [else (f (car lst) (my-foldr f base (cdr lst)))]))

(define (my-foldl f base lst)
  (cond [(empty? lst) base]
        [else (my-foldl f (f (car lst) base) (cdr lst))]))

(define (is-even x)
  (= (remainder x 2) 0))

(define (d/dx fun)
  (define δ (/ 1 100000))
  (λ(x)
    (/ (- (fun (+ x δ)) (fun (- x δ))) 2 δ)))
