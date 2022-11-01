#lang racket

(require 2htdp/image 2htdp/universe threading)


(define SIZE 30)
(define SEG-SIZE 30)
(define WIDTH-PX  (* SEG-SIZE SIZE))
(define HEIGHT-PX (* SEG-SIZE SIZE))
(define EXPIRATION-TIME 500)
(define TICK-RATE 1/10)

(define MT-SCENE
  (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "images/goo.gif"))
(define SEG-IMG  (bitmap "images/body.gif"))
(define HEAD-IMG (bitmap "images/head.gif"))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define ENDGAME-TEXT-SIZE 15)

(struct pit (snake goos) #:transparent)
(struct snake (dir segs) #:transparent)
(struct posn (x y) #:transparent)
(struct goo (loc expire) #:transparent)

(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

(define (next-pit w)
  (let* ([snake (pit-snake w)]
         [goos (pit-goos w)]
         [goo-to-eat (can-eat snake goos)])
    (if goo-to-eat
        (pit (grow snake) (age-goo (eat goos goo-to-eat)))
        (pit (slither snake) (age-goo goos)))))

(define (can-eat snake goos)
  (cond [(null? goos) #f]
        [else (if (close? (snake-head snake) (car goos))
                  (car goos)
                  (can-eat snake (cdr goos)))]))

(define (close? s g)
  (posn=? s (goo-loc g)))

(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

(define (grow sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (snake-segs sn))))

(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))))

(define (all-but-last segs)
  (take segs (- (length segs) 1)))

(define (next-head sn)
  (let* ([head (snake-head sn)]
         [dir (snake-dir sn)])
       (cond [(string=? dir "up") (posn-move head 0 -1)]
             [(string=? dir "down") (posn-move head 0 1)]
             [(string=? dir "left") (posn-move head -1 0)]
             [(string=? dir "right") (posn-move head 1 0)])))

(define (posn-move p dx dy)
  (posn (modulo (+ (posn-x p) dx) SIZE)
        (modulo (+ (posn-y p) dy) SIZE)))

(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME))

(define (make-fresh-goo x)
  (fresh-goo))

(define my-pit
  (pit (snake "right" (list (posn 1 1)))
      (make-list 6 make-fresh-goo)))

(define (age-goo goos)
  (rot (renew goos)))

(define (rot goos)
  (map decay goos))

(define (decay the-goo)
  (goo (goo-loc the-goo) (sub1 (goo-expire the-goo))))

(define (renew goos)
  (define (renew-if-rotten goo)
    (if (rotten? goo)
        (fresh-goo)
        goo))
  (map renew-if-rotten goos))

(define (rotten? g)
  (zero? (goo-expire g)))


(define (direct-snake w ke)
  (cond [(dir? ke) (world-change-dir w ke)]
        [else w]))

(define (dir? x)
  (or (key=? x "up")
      (key=? x "down")
      (key=? x "left")
      (key=? x "right")))

(define (world-change-dir w d)
  (define the-snake (pit-snake w))
  (cond [(opposite-dir? (snake-dir the-snake) d) w]
        [else
         (pit (snake-change-dir the-snake d) (pit-goos w))]))

(define (opposite-dir? d1 d2)
 (case (list d1 d2)
   [(("up" "down")
     ("down" "up")
     ("left" "right")
     ("right" "left")) #t]
   [else #f]))

(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))

(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (case dir
               [("up") HEAD-UP-IMG]
               [("down") HEAD-DOWN-IMG]
               [("left") HEAD-LEFT-IMG]
               [("right") HEAD-RIGHT-IMG])
             snake-body-scene))

(define snake-body
  (λ~>> snake-segs cdr))

(define snake-head
  (λ~>> snake-segs car))

(define (img-list+scene posns img scene)
  (cond [(null? posns) scene]
        [else (img+scene
               (car posns)
               img
               (img-list+scene (cdr posns) img scene))]))

(define (img+scene posn img scene)
  (place-image img
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))

(define (goo-list+scene goos scene)
  (img-list+scene (map goo-loc goos) GOO-IMG scene))

(define (dead? w)
  (define snake (pit-snake w))
  (self-colliding? snake))
 

(define (render-end w)
  (overlay (text "Game Over" ENDGAME-TEXT-SIZE "black")
           (render-pit w)))

(define (self-colliding? sn)
  (cons? (member (snake-head sn) (snake-body sn))))

(define (wall-colliding? sn)
  (define x (~>> sn snake-head posn-x))
  (define y (~>> sn snake-head posn-y))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))

(start-snake)
