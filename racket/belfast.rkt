#lang racket
(require "backend.rkt")


(init 10)

(define (ship p size)
  (let ([p0 p]
        [p1 (+x p (* size 2))]
        [p2 (+x p (* size -2))]
        )
    (let ([p10 (+z p1 (/ size 2))]
          [p11 (+x p1 (/ size 3))]
          [p12 (+y p1 (/ size 3))] 
          [p13 (+y p1 (/ size -3))])
      (point p10 0.5 0.0) (point (u0) 0.5 1.0 0.0 0.0) (point p1  0.5 0.0 0.5 0.0)
      (mirror (list (point (+pol p1 size pi/2) 0.5 0.0 0.5 1.0)(point (+pol p1 size 3pi/2) 0.5 0.0 0.5 1.0)(point (+pol p1 size 0.0) 0.5 0.0 0.5 1.0)
                    (irregularPyramid p1 (list (/ size 2) size (/ size 2)) (list pi/2 0.0 3pi/2) p10  0.0 0.5 1.0)
                    (irregularPyramid p1 (list (/ size 2) size (/ size 2)) (list pi/2 0.0 3pi/2) p10)) (u0) (ux))
      )))



(ship (u0) 10.0)
(time
 (begin
   (send_data)
   (thread while)))

(read)