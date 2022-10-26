#lang racket

(require 2htdp/image 2htdp/universe)
(require threading)

(struct state (x y direction speed trails) #:transparent)
(struct position (x y) #:transparent)

(define WIDTH 600)
(define HEIGHT 600)
(define UFO (bitmap "images/ufo.png"))
(define INITIAL-POSITION (state (/ WIDTH 2) 0 'down 3 '()))

(define (turn new-direction st)
  (struct-copy state st [direction new-direction]))

(define (change-speed inc/dec st)
  (define new-speed
   (inc/dec (state-speed st)))
  (struct-copy state st
               [speed (if (> new-speed 0) new-speed 0)]))

(define (control st key)
  (cond [(key=? key "up") (turn 'up st)]
        [(key=? key "down") (turn 'down st)]
        [(key=? key "left") (turn 'left st)]
        [(key=? key "right") (turn 'right st)]
        [(key=? key "+") (change-speed add1 st)]
        [(key=? key "-") (change-speed sub1 st)]
        [else st]))

(define (move st)
  (case (state-direction st)
    [(up)    (struct-copy state st [y (decrement-height st)])]
    [(down)  (struct-copy state st [y (increment-height st)])]
    [(left)  (struct-copy state st [x (decrement-width st)])]
    [(right) (struct-copy state st [x (increment-width st)])]
    [else st]))

(define (add-smoke st)
  (struct-copy state st [trails (take-at-most-20 (cons (smoke-position st) (state-trails st)))]))


(define (smoke-position st)
  (define offset-x (/ (image-width UFO) 2))
  (define offset-y (/ (image-height UFO) 2))
  (case (state-direction st)
    [(up) (position (state-x st) (+ (state-y st) offset-y))]
    [(down) (position (state-x st) (- (state-y st) offset-y))]
    [(left) (position (+ (state-x st) offset-x) (state-y st))]
    [(right) (position (- (state-x st) offset-x) (state-y st))]
    [else null]))

(define (tick)
  (λ~>> move add-smoke))



(define (change-within-limits comparison limit change-fn state-acc)
  (λ(st)
    (if (comparison (state-acc st) limit)
        limit
        (change-fn (state-acc st) (state-speed st)))))

(define increment-height
    (change-within-limits >= HEIGHT + state-y))
(define increment-width
    (change-within-limits >= WIDTH + state-x))
(define decrement-height
    (change-within-limits <= 0 - state-y))
(define decrement-width
    (change-within-limits <= 0 - state-x))

(define (draw-a-ufo-onto-an-empty-scene st)
  (place-image UFO (state-x st) (state-y st)
               (empty-scene WIDTH HEIGHT)))

(define (take-at-most-20 ls)
  (if (> (length ls) 20)
      (take ls 20)
      ls))

(define (show-smoke ls scene)
  (define (circle-size index)
    (* 2 (+ index 1)))
    

  (define (render pos current-scene)
    (place-image
     (circle (~>> pos car circle-size) "outline" "gray")
     (~>> pos cdr position-x)
     (~>> pos cdr position-y)
     current-scene))
  (foldl render scene ls))

(define (indexed ls)
  (map cons (range (length ls)) ls))

#;
(show-smoke (indexed (list (position 10 10) (position 20 20) (position 30 30))) (empty-scene WIDTH HEIGHT))

(big-bang INITIAL-POSITION
         (on-tick (λ~>> add-smoke move))
         (to-draw (λ(st) (show-smoke (~>> st state-trails indexed) (draw-a-ufo-onto-an-empty-scene st))))
         (on-key control)
         (stop-when (\λ(x) #f)))
