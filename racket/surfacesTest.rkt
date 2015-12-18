#lang racket
(require ffi/unsafe
         ffi/unsafe/define
         math/flonum
         math/matrix
         "sliders.rkt"
         "base.rkt"
         (prefix-in p3d: pict3d)
         )

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


(point (u0) 1.0 0.0 0.0)

(time
 (begin
   (send_data)
   (thread while)))

(read)