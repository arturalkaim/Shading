#lang racket
(require ffi/unsafe
         ffi/unsafe/define
         math/flonum
         math/matrix
         "sliders.rkt"
         "base.rkt"
         (prefix-in p3d: pict3d)
         )

(define (vpol rho phi)
  (p3d:dir (* rho (cos phi))
           (* rho (sin phi))
           0.0))
#;(define (unitize v)
    (p3d:dir-normalize v))

(define (v*v v1 v2)
  (p3d:dir-cross v1 v2))

;(p3d:dir-dy rot-dir) (p3d:dir-dz rot-dir)



(define (vector-normalizado v)
  (let ((l (sqrt (+ (sqr (cx v))
                    (sqr (cy v))
                    (sqr (cz v))))))
    (xyz (/ (cx v) l)
         (/ (cy v) l)
         (/ (cz v) l))))

(define (produto-cruzado p0 p1)
  (xyz (* (- (cy p0) (cy p1)) (+ (cz p0) (cz p1)))
       (* (- (cz p0) (cz p1)) (+ (cx p0) (cx p1)))
       (* (- (cx p0) (cx p1)) (+ (cy p0) (cy p1)))))

(define (produtos-cruzados pts)
  (if (null? (cdr pts))
      (xyz 0 0 0)
      (+c (produto-cruzado (car pts) (cadr pts))
          (produtos-cruzados (cdr pts)))))

(define (normal-poligono pts)
  (vector-normalizado
   (produtos-cruzados
    (append pts (list (car pts))))))

(define (normal-quadrangulo p0 p1 p2 p3)
  (normal-poligono (list p0 p1 p2 p3)))


(define (media-pontos p0 p1)
  (xyz (/ (+ (cx p0) (cx p1)) 2)
       (/ (+ (cy p0) (cy p1)) 2)
       (/ (+ (cz p0) (cz p1)) 2)))

(define (centro-quadrangulo p0 p1 p2 p3)
  (media-pontos
   (media-pontos p0 p2)
   (media-pontos p1 p3)))

(define (sinusoide a omega fi x)
  (* a (sin (+ (* omega x) fi))))

(define (itera-quadrangulos f ptss)
  (for/list ((pts0 ptss)
             (pts1 (cdr ptss)))
    (for/list ((p0 pts0)
               (p1 pts1)
               (p2 (cdr pts1))
               (p3 (cdr pts0)))
      (f p0 p1 p2 p3))))



(define (tijolo  p0 p1 p2 p3)
  (let ((c (* 1.0 (distance p0 p1))) 
        (h (/ (distance p0 p3) 2))
        (l (* 0.6 (distance p0 p1)))
        (n (normal-quadrangulo p0 p1 p2 p3))
        (p (centro-quadrangulo p0 p1 p2 p3)))
    (let ((p01 (media-pontos p0 p))
          (p02 (media-pontos p p2)))
      (let ((pp1 (+c p01 (*c n l)))
            (pp2 (+c p02 (*c n l))))
        (right-cuboid p01 c h pp1 0.9 0.9 0.9)
        (right-cuboid pp2 c h p02 0.9 0.9 0.9)))))

(define maxy (make-parameter 7))

(define (malha-teste minx maxx miny maxy contx conty)
  (map-division (lambda (i j)                              
                  (xyz i (* j (sinusoide 0.15 1.75 0 i)) j))
                ;(xyz i (* j (sinusoide 0.2 1.5 0 i)) (+ j (* 1.5 (sinusoide 0.3 1.5 0 i)))))
                minx maxx contx
                miny maxy conty)
  )
(define (brick-wall minx maxx miny maxy contx conty)
  (itera-quadrangulos tijolo (malha-teste  minx maxx miny maxy contx conty)))


(setup brick-wall (list -21 21 0 10 25 12))

(time
 (begin
   (send_data)
   (thread while)))


(sliders
 "Brick Wall"
 (lambda (minx maxx miny maxy contx conty)
   (when (and (<= miny maxy) (< minx maxx)) 
     (update (list  minx maxx miny maxy contx conty))
     ))
 '(("minx" -30 30 -20)
   ("maxx" 0 30 20)
   ("miny" -10 10 0)
   ("maxy" 0 10 5)
   ("contx" 1 100 5)
   ("conty" 1 50 5)))





;(itera-quadrangulos tijolo (malha-teste -21 21 0 10))
;(right-cuboid (xyz 0.0 0.0 0.0) 1.0 1.0 (xyz 2.0 0.0 0.0) 0.9 0.9 0.9)
;(right-cuboid (xyz 0.0 0.0 2.0) 1.0 1.0 (xyz 2.0 2.0 2.0) 0.9 0.9 0.9)
;(right-cuboid (xyz 0.0 0.0 4.0) 1.0 1.0 (xyz 0.0 2.0 4.0) 0.9 0.9 0.9)
;(right-cuboid (xyz 2.0 0.0 0.0) 1.0 1.0 (xyz 3.0 1.0 0.0) 0.9 0.9 0.9)
;(right-cuboid (xyz 2.0 (- 1.0) 0.0) 1.0 1.0 (xyz 2.5 1.0 0.0) 0.9 0.9 0.9)
;(prism-p (xyz (- 2.0) (- 1.0) 0.0) 2.0 1.0 (xyz (- 2.5) 1.0 0.0) 4.0 0.0 1.0 0.5)
;(prism-p (xyz (- 2.0) (- 1.0) 10.0) 2.0 1.0 (xyz (- 2.5) 1.0 10.0) 4.0 0.0 1.0 0.5)



(displayln "END")


