#lang racket

(require 2htdp/universe 2htdp/image)

(struct interval (small big did-win rounds-played) #:transparent)

(define HEIGHT 300)
(define WIDTH 500)
(define TEXT-X 0)
(define PADDING 10)
(define TEXT-UPPER-Y PADDING)
(define TEXT-LOWER-Y (- HEIGHT PADDING))
(define TEXT-SIZE 20)
(define SIZE 50)
(define TEXT-COLOR "blue")
(define COLOR "red")
(define HELP-TEXT
  (text "↑ for larger numbers, ↓ for smaller ones"
        TEXT-SIZE
        TEXT-COLOR))
(define HELP-TEXT2
  (text "Press = when your number is guessed; q to quit"
        TEXT-SIZE
        TEXT-COLOR))
(define (center-x image)
  (- (/ WIDTH 2) (/ (image-width image) 2)))
(define MT-SC
  (place-image/align
   HELP-TEXT (center-x HELP-TEXT) TEXT-UPPER-Y "left" "top"
   (place-image/align
    HELP-TEXT2 (center-x HELP-TEXT2) TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))

(define (start lower upper)
  (big-bang (interval lower upper #f 0)
            (on-key deal-with-guess)
            (to-draw render)
            (stop-when single? render-last-scene)))

(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with-win w)]
        [else w]))


(define (guess w)
  (quotient (+ (interval-small w) (interval-big w)) 2))

(define (render w)
  (render-tries w (overlay (text (number->string (guess w)) SIZE COLOR) MT-SC)))

(define (render-tries w image)
  (define tries-text
    (text (string-append "Tries: " (number->string (interval-rounds-played w))) 20 "black"))
  (place-image/align tries-text (center-x tries-text) (+ (* 2 PADDING) (image-height tries-text)) "left" "top" image))

(define (render-last-scene w)
  (define end-text
    (if (interval-did-win w)
        (string-append "I WIN, GUESS: " (number->string (guess w)))
        "END"))
  (overlay (text end-text SIZE COLOR) MT-SC))

(define (single? w)
  (= (interval-small w) (interval-big w)))

(define (stop-with-win w)
  (define new-value
    (struct-copy interval w (did-win #t)))
  (stop-with new-value))

(define (smaller w)
  (struct-copy interval w (big (max (interval-small w) (sub1 (guess w))))
                          (rounds-played (add1 (interval-rounds-played w)))))

(define (bigger w)
  (struct-copy interval w (small (min (interval-big w) (add1 (guess w))))
                          (rounds-played (add1 (interval-rounds-played w)))))
