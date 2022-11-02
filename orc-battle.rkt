#lang racket

(require rackunit 2htdp/universe 2htdp/image threading)

(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)

(struct orc-world (player lom attack# target) #:transparent #:mutable)

(struct player (health agility strength) #:transparent #:mutable)

(struct monster ([health #:mutable]) #:transparent)
(struct orc monster (club) #:transparent)
(struct hydra monster () #:transparent)
(struct slime monster (sliminess) #:transparent)
(struct brigand monster () #:transparent)

(define (player-update! setter selector mx)
  (λ(player delta)
    (setter player (interval+ (selector player) delta mx))))

(define player-health+
  (player-update! set-player-health! player-health MAX-HEALTH))
(define player-agility+
  (player-update! set-player-agility! player-agility MAX-AGILITY))
(define player-strength+
  (player-update! set-player-strength! player-strength MAX-STRENGTH))

(define (interval+ n m max-value)
  (define sum (+ n m))
  (~>> (+ n m) (min max-value) (max 0)))
  

;; Game

(define (start)
  (big-bang (initialize-orc-world)
            (on-key player-acts-on-monsters)
            (to-draw render-orc-battle)
            (stop-when end-of-orc-battle? render-the-end)))

(define (initialize-orc-world)
  (define player-0 (initialize-player))
  (define lom0 (initialize-monsters))
  (orc-world player0 lom0 (random-number-of-attacks player0) 0))

(define (end-of-orc-battle? w)
  (or (win? w) (lose? w)))

(define (render-orc-battle w)
  (render-orc-world w (orc-world-target w) (instructions w)))



;; Tests

(check-eq? (interval+ 20 20 50) 40)
(check-eq? (interval+ 20 20 30) 30)
(check-eq? (interval+ 20 -30 50) 0)
(check-eq? (interval+ -30 20 50) 0)

(check-equal? (let ((p (player 1 2 3)))
                (player-strength+ p -3)
                p)
              (player 1 2 0))
