#lang racket
(require rosetta/autocad)

;;#lang rosetta-racket-debug


(provide spatial-truss
         spatial-truss-insert-apex)

; spatial truss

(define truss-knot-radius 0.2)
(define truss-knot sphere)
(define (truss-knots cs radius) (map (λ (c) (truss-knot c radius)) cs))

(define truss-bar-radius 0.07)
(define truss-bar cylinder)
(define (truss-bars cs1 cs2 radius) (map (λ (c1 c2) (truss-bar c1 radius c2)) cs1 cs2))

(define (spatial-truss curves (knot-radius truss-knot-radius) (bar-radius truss-bar-radius))
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
   (* (- (xyz-y c1) (xyz-y c2)) (+ (xyz-z c1) (xyz-z c2)))
   (* (- (xyz-z c1) (xyz-z c2)) (+ (xyz-x c1) (xyz-x c2)))
   (* (- (xyz-x c1) (xyz-x c2)) (+ (xyz-y c1) (xyz-y c2)))))

(define (cross-products cs)
  (if (empty? (rest cs))
      (u0)
      (+c
       (cross-product (first cs) (second cs))
       (cross-products (rest cs)))))

(define (polygon-normal cs)
  (*c
   (norm-c
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
        (spatial-truss (insert-pyramid-apex-curves cs) knot-radius bar-radius)))))


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
 ; (view
  ; (xyz 29.793292940564022 29.507387102551313 29.51671165317999)
   ;(xyz 0.2765812873840332 -0.00932455062867632 0.0)
   ;(perspective 197)
   (moebius-truss 1 0 (* pi 4) 80 0 0.3 1));)


(sliders
 "Moebius Truss"
 (lambda (r u0 u1 m v0 v1 n)
   (moebius-truss r (* u0 pi) (* u1 pi) m v0 v1 n))
 '(("R" 0 8 1)
   ("U0" 0 2)
   ("U1" 1 4)
   ("M" 1 50 10)
   ("V0" 0 1)
   ("V1" 1 2)
   ("N" 1 5)))
