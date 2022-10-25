#lang racket

(require rackunit)
(require 2htdp/image)

(define (my-length a-list)
  (if (null? a-list)
      0
      (add1 (my-length (cdr a-list)))))

(define (all? condition-list x)
  (eval (cons 'and (map (λ (f) (f x)) condition-list))))

(all? (list (curryr > 2) (curryr < 10)) 5)

(struct point (x y) #:transparent)

(define (distance-to-origin p)
  (sqrt (+ (sqr (point-x p))
           (sqr (point-y p)))))

(define pt1 (point -1 2))
(define pt2 (point -1 2))

(equal? pt1 pt2)

;; Chapter 4 1/2

(define WIDTH 100)
(define HEIGHT 200)

(define X-CENTER (quotient WIDTH 2))
(define Y-CENTER (quotient HEIGHT 2))

(unless (> HEIGHT 0)
  (error 'guess-my-number "HEIGHT may not be negative"))

(define SQR-COLOR "red")
(define SQR-SIZE 10)
(define (draw-square img x y)
  (place-image (square SQR-SIZE "solid" SQR-COLOR)
               x y
               img))

(struct posn (x y))
(struct rectangle (width height))
(define (inside-of-rectangle? r p)
  (define x (posn-x p))
  (define y (posn-y p))
  (define width (rectangle-width r))
  (define height (rectangle-height r))
  (and (<= 0 x) (< x width) (<= 0 y) (< y height)))
