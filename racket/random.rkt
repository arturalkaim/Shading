#lang racket
(require rosetta/rhino)

(define (random-point)
  (xyz (random 100) (random 100) 0))

(for ((range 100))
  (sphere (random-point) 3))