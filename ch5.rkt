#lang racket

(require 2htdp/universe 2htdp/image)

(define WIDTH 200)
(define HEIGHT 300)
(define IMAGE-OF-UFO (bitmap "images/ufo.png"))
(define FINAL-STATE (- HEIGHT (/ (image-height IMAGE-OF-UFO) 2)))

(define (add-3-to-state current-state)
  (+ current-state 3))

(define (draw-a-ufo-onto-an-empty-scene current-state)
  (place-image IMAGE-OF-UFO (/ WIDTH 2) current-state
               (empty-scene WIDTH HEIGHT)))

(define (state-is-final current-state)
  (>= current-state FINAL-STATE))

#; (big-bang 0
          (on-tick add-3-to-state)
          (to-draw draw-a-ufo-onto-an-empty-scene)
          (stop-when state-is-final))
