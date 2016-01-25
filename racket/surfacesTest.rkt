#lang racket
(require rosetta/glfast)

(init 10)

#;(let* ([p0 (u0)]
         [p1 (+z p0 5.0)])
    (reg-surface p0 12.0 20.0 20.0 p1)
    )
(let* ([p0 (xyz 0.0 0.0 0.0)]
       [p1 (+y p0 5.0)]
       [p2 (+xy p0 2.1 5.0)]
       [p3 (+xy p0 -2.1 5.0)])
  ;  (reg-surface p0 7.0 2.0 2.0 p1)
  
  (reg-line p0 7.0 4.0 4.0 p1 0.0 0.0 0.0 1.0)
  (reg-line p0 7.0 4.0 4.0 p2 90 1.0 0.0 0.0)
  (reg-surface p0 7.0 4.0 4.0 p3 pi/2 0.0 1.0 0.0)
  )


#;(let* ([p0 (u0)]
         [p1 (+z p0 15.0)])
    (trunk p0 5.0 5.0 17.0 17.0 p1 5.0))

(let* ([p0 (xyz 0.0 5.0 0.0)]
       [p1 (+yz p0 0.0 20.0)]
       [a1 (/ pi 2)]
       [a2 (* (/ pi 6) 7)]
       [a3 (- (/ pi 3))])
  (irregularPyramid p0 p1 35.0 a1 37.0 a2 37.0 a3 0.0 0.7 0.0))

(point (u0) 1.0 0.0 0.0)

(time
 (begin
   (send_data)
   (thread while)))

(read)