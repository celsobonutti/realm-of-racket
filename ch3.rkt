#lang racket

(require threading)

(struct student (name id# dorm) #:transparent)

(define freshman1
  (student 'Joe 1234 'NewHall))

(define freshman2
  (student 'Joe 1234 'NewHall))

(define carlos (student 'Carlos 100 'Memes))
(define eri (student 'Eri 25 'Basimga))
(define romualdo (student 'Romualdo 150 'Memes))
(define roberto (student 'Roberto 20 'Xd))

(define students (list carlos eri romualdo roberto))

(define memes-names
  (λ~>>
    (filter (λ~> student-dorm (eq? 'Memes)))
    (map student-name)))
