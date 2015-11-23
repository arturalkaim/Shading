#lang racket
(require ffi/unsafe
         ffi/unsafe/define
         math/flonum
         (except-in  "base.rkt" sphere cylinder box)
         (prefix-in p3d: pict3d)
         racket/trace
         )

(define-ffi-definer define-test (ffi-lib "/Users/arturalkaim/Shaders/lib/libShaders"))

(define-test start (_fun -> _int))
(define-test createPoints (_fun _int -> _int))
(define-test box (_fun _float _float _float _float _float _float -> _int))
(define-test cylinder (_fun _float _float _float _float _float _float _float _float _float _float _float _float -> _int))
(define-test sphere (_fun _float _float _float _float _float _float _float -> _int))
(define-test pyramid (_fun _float _float _float _float _float _float _float -> _int))
(define-test rotate (_fun _int _float _float _float _float -> _int))
(define-test move (_fun _int _float _float _float -> _int))
(define-test scale (_fun _int _float _float _float -> _int))
(define-test init (_fun _int -> _int))
(define-test cycle (_fun -> _void))
(define-test pool (_fun -> _void))
(define-test main (_fun -> _int))
(define-test end_cycle (_fun -> _int))
(define-test send_data (_fun -> _int))
(define-test city (_fun _int -> _void))


(define pi/2 (/ pi 2))

(define (box-p p l w h)
  (box (cx p) (cy p) (cz p) l w h))
(define (cube-p p l)
  (box (cx p) (cy p) (cz p) l l l))
(define (sphere-p p r)
  (sphere (cx p) (cy p) (cz p) r 0.0 0.0 0.0))
(define (cylinder-z p l h ang vx vy vz)
  (cylinder (p3d:pos-x p) (p3d:pos-y p) (p3d:pos-z p) l h 0.0 0.0 0.0 ang vx vy vz))

(define (cylinder-p p0 h p1)
  (if (not (xyz? p1))
      (cylinder-z p0 h (/ p1 2 ))
      (let* ((vec-dir (p3d:pos- (xyz->pos p1) (xyz->pos p0)))
             (rot-dir (p3d:dir-normalize (p3d:dir-cross vec-dir p3d:+z)))
             (dir (p3d:dir h h (/ (p3d:dir-dist vec-dir) 2)))
             (center (p3d:pos-between (xyz->pos p0) (xyz->pos p1) 1/2))
             (angle (acos (/ (p3d:dir-dot vec-dir p3d:+z) (* (p3d:dir-dist vec-dir) (p3d:dir-dist p3d:+z))))))
        (match-let-values
         ([(yaw pit) (p3d:dir->angles (p3d:pos- (xyz->pos p1) (xyz->pos p0)))])
         (cylinder-z center (p3d:dir-dx dir) (p3d:dir-dist vec-dir) (- angle) (p3d:dir-dx rot-dir) (p3d:dir-dy rot-dir) (p3d:dir-dz rot-dir))
         ))
      )
  )

(define (itera-pts f ptss)
  (for/list ((pts0 ptss))
    (for/list ((p0 pts0))
      (f p0))))

(define (itera-quads f ptss)
  (for/list ((pts0 ptss)
             (pts1 (cdr ptss)))
    (for/list ((p0 pts0)
               (p1 pts1)
               (p2 (cdr pts1))
               (p3 (cdr pts0)))
      (f p0 p1 p2 p3))))


(define (media a b)
  (/ (+ a b) 2.0))

(define (media-pontos p0 p1)
  (xyz (media (cx p0) (cx p1))
       (media (cy p0) (cy p1))
       (media (cz p0) (cz p1))))

(define (centro-quadrangulo p0 p1 p2 p3)
  (media-pontos
   (media-pontos p0 p2)
   (media-pontos p1 p3)))

(define (average-points ptss)
  (itera-quads centro-quadrangulo ptss))

(define (itera-quads-alternated f ptss)
  (append (itera-quads f ptss)
          (itera-quads f (average-points ptss))))

(define (pts-from-domain surf u0 u1 n v0 v1 m)
  (map-division surf u0 u1 n v0 v1 m))

(define (centered-pts-from-domain surf u0 u1 n v0 v1 m)
  (let ((du (/ (- u1 u0) n 2))
        (dv (/ (- v1 v0) m 2)))
    (map-division surf 
                  (+ u0 du) (- u1 du) (- n 1)
                  (+ v0 dv) (- v1 dv) (- m 1))))

(define (itera-quads-from-domain f surf u0 u1 n v0 v1 m)
  (itera-quads f (pts-from-domain surf u0 u1 n v0 v1 m)))

(define (itera-centered-quads-from-domain f surf u0 u1 n v0 v1 m)
  (itera-quads f (centered-pts-from-domain surf u0 u1 n v0 v1 m)))

(define (itera-quads-alternated-from-domain f surf u0 u1 n v0 v1 m)
  (append
   (itera-quads-from-domain f surf u0 u1 n v0 v1 m)
   (itera-centered-quads-from-domain f surf u0 u1 n v0 v1 m)))

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

(define (ponto-intermedio p0 p1 f)
  (+c p0 (*c (-c p1 p0) f)))

(define (itera-quads-chess f ptss)
  (for/list ((pts0 ptss)
             (pts1 (cdr ptss))
             (start-on? (in-cycle (list #t #f))))
    (for/list ((p0 pts0)
               (p1 pts1)
               (p2 (cdr pts1))
               (p3 (cdr pts0))
               (on? (if start-on? 
                        (in-cycle (list #t #f))
                        (in-cycle (list #f #t)))))
      (if on?
          (f p0 p1 p2 p3)
          (list)))))

(define (map-division-recursive f u0 u1 n v0 v1 m recurse? [rec 0])
  (let ((du (/ (- u1 u0) n))
        (dv (/ (- v1 v0) m)))
    (map-division (lambda (u v)
                    (if (recurse? u v rec)
                        (map-division-recursive f u (+ u du) 2 v (+ v dv) 2 recurse? (+ rec 1))
                        (f u v du dv)))
                  u0 u1 n #f v0 v1 m #f)))




(define (combine m0 m1)
  (map (lambda (r0 r1)
         (map cons r0 r1))
       m0
       m1))

(define (itera-quadrangulos-cores f pts cores)
  (itera-quads (lambda (pc0 pc1 pc2 pc3)
                 (f (car pc0) (car pc1) (car pc2) (car pc3)
                    (cdr pc0) (cdr pc1) (cdr pc2) (cdr pc3)))
               (combine pts cores)))

(define atrator1 (xyz 0 0 0))


;MALHA
(define e 0.12)
(define c 18)
(define a 12)
(define n 20)
(define m 20)
(define (my-surf i j)
  (xyz i 0 j)) 
(define (malha)
  (map-division my-surf
                0 (+ c e) n
                0 a m))
(define (my-surf2 i j)
  (xyz 0 i j)) 
(define (malha2)
  (map-division my-surf2
                0 c n
                0 a m))
(define (my-surf3 i j)
  (xyz c i j)) 
(define (malha3)
  (map-division my-surf3
                0 c n
                0 a m))




(define (cils p0 p1 p2 p3)
  (let* ((p (centro-quadrangulo p0 p1 p2 p3))
         (n (normal-quadrangulo p0 p1 p2 p3))
         (r (+ (* 0.99 (/ (distance p0 p1) 2.5)) (* 0.10 (cos (* (/ (distance p0 atrator1) 10) pi))))))
    (cylinder-p p r (+c p (*c n 0.5)))))

#;(union
   (with-current-layer "vidro1" (itera-quads cils (malha)))
   (surface-grid (malha))
   (with-current-layer "vidro2" (itera-quads cils (malha3)))
   (surface-grid (malha3)))

(init 800)
(itera-quads cils (malha))
(send_data)

(time 
  (begin 
    (start)
    ))

