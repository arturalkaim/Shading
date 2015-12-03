#lang racket
(require ffi/unsafe
         ffi/unsafe/define
         math/flonum
         "base.rkt"
         (prefix-in p3d: pict3d)
         racket/trace
         "sliders.rkt"
         )

(define truss-knot-radius (make-parameter 0.2))
(define truss-knot sphere)
(define (truss-knots cs radius) (map (λ (c) (truss-knot c radius)) cs))

(define truss-bar-radius (make-parameter 0.07))
(define truss-bar cylinder)
(define (truss-bars cs1 cs2 radius) (map (λ (c1 c2) (truss-bar c1 radius c2)) cs1 cs2))

(define (spatial-truss curves (knot-radius (truss-knot-radius)) (bar-radius (truss-bar-radius)))
  (let ((as (first curves))
        (bs (second curves))
        (cs (third curves)))
    (list
     (truss-knots as knot-radius)
     (truss-knots bs knot-radius)
     (truss-bars as cs bar-radius)
     (truss-bars bs (drop-right as 1) bar-radius)
     (truss-bars bs (drop-right cs 1) bar-radius)
     (truss-bars bs (rest as) bar-radius)
     (truss-bars bs (rest cs) bar-radius)
     (truss-bars (rest as) (drop-right as 1) bar-radius)
     (truss-bars (rest bs) (drop-right bs 1) bar-radius)
     (if (empty? (cdddr curves))
         (list
          (truss-knots cs knot-radius)
          (truss-bars (rest cs) (drop-right cs 1) bar-radius))
         (list
          (truss-bars bs (first (drop curves 3)) bar-radius)
          (spatial-truss (drop curves 2) knot-radius bar-radius))))))


; spatial truss insert apex

(provide spatial-truss-insert-apex)


; utils

(define (cross-product c1 c2)
  (xyz
   (* (- (cy c1) (cy c2)) (+ (cz c1) (cz c2)))
   (* (- (cz c1) (cz c2)) (+ (cx c1) (cx c2)))
   (* (- (cx c1) (cx c2)) (+ (cy c1) (cy c2)))))

(define (cross-products cs)
  (if (empty? (rest cs))
      (u0)
      (+c
       (cross-product (first cs) (second cs))
       (cross-products (rest cs)))))

(define (polygon-normal cs)
  (*c
   (unitize
    (cross-products
     (append cs (list (car cs)))))
   -1))

(define (midcoord p0 p1)
  (/c (+c p0 p1) 2))

(define (quad-center c1 c2 c3 c4)
  (midcoord (midcoord c1 c3) (midcoord c2 c4)))

(define (quad-normal c1 c2 c3 c4)
  (polygon-normal (list c1 c2 c3 c4)))

(define (quad-pyramid-apex c1 c2 c3 c4)
  (let ((h 
         (/
          (+
           (distance c1 c2)
           (distance c2 c3)
           (distance c3 c4)
           (distance c4 c1))
          4
          (sqrt 2))))
    (+c (quad-center c1 c2 c3 c4)
        (*c (quad-normal c1 c2 c3 c4) h))))


(define (insert-pyramid-apex-2-curves cs1 cs2)
  (cons
   (quad-pyramid-apex (first cs1) (first cs2) (second cs2) (second cs1))
   (if (empty? (cddr cs1))
       (list)
       (insert-pyramid-apex-2-curves (rest cs1) (rest cs2)))))

(define (insert-pyramid-apex-curves curves)
  (if (empty? (rest curves))
      curves
      (cons
       (first curves)
       (cons (insert-pyramid-apex-2-curves (first curves) (second curves))
             (insert-pyramid-apex-curves (rest curves))))))

(define (spatial-truss-insert-apex cs)
  (let ((c1 (first (first cs)))
        (c2 (first (second cs)))
        (c4 (second (first cs))))
    (let ((d (min (distance c1 c2) (distance c1 c4))))
      (let ((knot-radius (/ d 5))
            (bar-radius (/ d 15)))
        (spatial-truss (insert-pyramid-apex-curves cs)#;#; knot-radius bar-radius)))))


(define (enumerate-n fn a b n)
  (map-in-interval fn a b n))

(define (enumerate-m-n fn u1 u2 m v1 v2 n)
  (enumerate-n
   (λ (u) (enumerate-n (λ (v) (fn u v)) v1 v2 n))
   u1 u2 m))


; moebius truss

(define (moebius-cs r u1 u2 m v1 v2 n)
  (enumerate-m-n
   (λ (u v)
     (cyl (* r (+ 1 (* v (cos (/ u 2)))))
          u
          (* r (* v (sin (/ u 2))))))
   u1 u2 m v1 v2 n))

(define (moebius-truss r u1 u2 m v1 v2 n)
  (spatial-truss-insert-apex (moebius-cs r u1 u2 m v1 v2 n)))

(define (moebius-truss-example)
   (moebius-truss 3 0 (* pi 4) 80 0 0.3 1));)

(displayln "START")




;(moebius-truss-example)

(setup moebius-truss (list  3 0 (* pi 4) 20 0 0.3 1))

(time 
 (begin 
   (send_data)
   (thread while)
   ))

(sliders
 "Moebius Truss"
 (lambda (rk rb r u0 u1 m v0 v1 n)
   ;(displayln (list r u0 u1 m v0 v1 n))
   (parameterize ((truss-knot-radius rk)
                  (truss-bar-radius rb))
     (update (list r (* u0 pi) (* u1 pi) m v0 v1 n)))
   )
 '(("RK" 0 10 1)
   ("RB" 0 10 1)
   ("R" 0 8 3)
   ("U0" 0 2)
   ("U1" 1 15 12)
   ("M" 1 100 20)
   ("V0" 0 3)
   ("V1" 1 3)
   ("N" 1 10 1)))