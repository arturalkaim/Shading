#lang racket
(require ffi/unsafe
         ffi/unsafe/define
         math/flonum
         math/matrix
         "sliders.rkt"
         "base.rkt"
         (prefix-in p3d: pict3d)
         )

(define (test a)
  (displayln a))

(define (star p0 size h)
  (let* ((p1 p0)
         (p2 (+y p1 (* h 2))))
    (pyramid p1 size size p2 3.0 1.0 1.0 0.0)
    (rotate (pyramid p1 size size p2 3.0 1.0 1.0 0.0) (/ pi 3) (xyz 0.0 0.0 1.0))
    ))
(define (tree p0 size h [n 4])
  (let* ((p1 (+z p0 (- size)))
         (p2 (+z p1 size)))
    (when (> n 0)
      (begin (pyramid p1 size size p2 5.0 85 107 47)
             (tree (+z p0 (/ size 5.0)) (/ size 2) h (- n 1)))
      )))

(define (christmas-tree p0 size h)
  ;(star (+z p0 size) (* size 0.8) h)
  ;(star (+z p0 size) (* size 0.8) (- h))
  ;(rotate (prism (+z p0 (* (- size) 4)) size size (+z p0 (* (- size) 2)) 7.0 128.0 50.0 0.0) (/ pi 4) (xyz 0.0 0.0 1.0))
  ;(tree p0 (* size 3) h 4.0)
  (trunkpts (u0) 15.0 15.0 13.0 13.0 (+z (u0) 30) 5.0)
  )

(init 10)
;(setup christmas-tree (list (u0) 3.0 0.3))

(define (animate)
  (for
      ([n (in-range 5 200 0.1)])
    (begin (sleep 0.05)
     (update (list (pol 5.0 n) 3.0 0.1)))
    ))


(christmas-tree (u0) 3.0 0.1)
#;(let ((size 3.0)
      (h 2.0))
  (pyramid (+z (u0) (- h)) size size (+z (u0) h) 30.0  200 17 27))

(time
 (begin
   (send_data)
   (thread while)))

#;(animate)
#;(sliders
 "Truss"
 (lambda (n sizex sizey) (update (list (pol sizey n) sizex 0.1)))
 '(("n" 5 50 20)
   ;("sides" 3 20 5)
   ("sizex" 1 20 5)
   ("sizey" 1 20 10)))