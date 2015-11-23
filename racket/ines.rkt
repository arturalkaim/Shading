#lang racket
(require ffi/unsafe
         ffi/unsafe/define
         math/flonum
         math/matrix
         "sliders.rkt"
         (except-in  "base.rkt" sphere cylinder box right-cuboid)
         (prefix-in p3d: pict3d)
         racket/trace
         "fffidefiner.rkt"
         )
;(define shading-lib-path (build-path "C:\\Users\\DEMO\\Documents\\Visual Studio 2015\\Projects\\shading\\x64\\Debug\\shading"))
(define shading-lib-path "shading")
(displayln "teste")
(displayln shading-lib-path)
#;(define shading-lib (ffi-lib shading-lib-path #:fail (λ () (displayln "FAIL!!!"))))
(define shading-lib null)
(define-ffi-definer define-f-function shading-lib)
(unless (ffi-lib? shading-lib)
  (displayln "FAIL LIB FAIL LIB FAIL LIB!!!"))

;(defffi "int box(float pos_x, float pos_y, float pos_z, float w, float l, float h, float red, float g, float b, float angle, float vx, float vy, float vz)")

(define-f-function start (_fun -> _int))
;(define-f-function box (_fun _float _float _float _float _float _float _float _float _float _float _float _float _float -> _int))
(define-f-function cylinder (_fun _float _float _float _float _float _float _float _float _float _float _float _float -> _int))
(define-f-function prism (_fun _float _float _float _float _float _float _float _float _float _float _float _float _float _float -> _int))
(define-f-function prismpts (_fun _float _float _float _float _float _float _float _float _float _float _float _float _float -> _int))
(define-f-function sphere (_fun _float _float _float _float _float _float _float -> _int))
(define-f-function pyramid (_fun _float _float _float _float _float _float _float -> _int))
(define-f-function rotate (_fun _int _float _float _float _float -> _int))
(define-f-function move (_fun _int _float _float _float -> _int))
(define-f-function scale (_fun _int _float _float _float -> _int))
(define-f-function init (_fun _int -> _int))
(define-f-function cycle (_fun -> _void))
(define-f-function pool (_fun -> _void))
(define-f-function end_cycle (_fun -> _int))
(define-f-function send_data (_fun -> _int))



(define (end_cycle?)
  (if (< (end_cycle) 0)
      #f
      #t))

(define (box-p p l w h ang vx vy vz)
  (box (cx p) (cy p) (cz p) l w h 0.0 0.0 0.0 ang vx vy vz))
(define (cube-p p l)
  (box (cx p) (cy p) (cz p) l l l))
(define (sphere-p p r)
  (sphere (cx p) (cy p) (cz p) r 0.0 0.0 0.0))
(define (cylinder-z p l h r g b ang vx vy vz)
  (cylinder (p3d:pos-x p) (p3d:pos-y p) (p3d:pos-z p) l h r g b ang vx vy vz))
(define (prism-z p l w h sides r g b ang vx vy vz)
  (prism (p3d:pos-x p) (p3d:pos-y p) (p3d:pos-z p) l w h sides r g b ang vx vy vz))

#;(define (prism-p p0 w h p1 sides r g b)
  (if (not (xyz? p1))
      (cylinder-z p0 h (/ p1 2 ))
      (let* ((vec-dir (p3d:pos- (xyz->pos p1) (xyz->pos p0)))
             (rot-dir (p3d:dir-normalize (p3d:dir-cross vec-dir p3d:+y)))
             (dir (p3d:dir w h (/ (p3d:dir-dist vec-dir) 2)))
             (center (p3d:pos-between (xyz->pos p0) (xyz->pos p1) 1/2))
             (angle-x (acos (/ (p3d:dir-dot vec-dir p3d:+x) (* (p3d:dir-dist vec-dir) (p3d:dir-dist p3d:+x)))))
             (angle-z (acos (/ (p3d:dir-dot vec-dir p3d:+y) (* (p3d:dir-dist vec-dir) (p3d:dir-dist p3d:+y)))))
             (angle-y (acos (/ (p3d:dir-dot vec-dir p3d:+z) (* (p3d:dir-dist vec-dir) (p3d:dir-dist p3d:+z))))))
        (displayln angle-x)
        (displayln angle-y)
        (displayln angle-z)
        (displayln "")
        (match-let-values
         ([(yaw pit) (p3d:dir->angles (p3d:pos- (xyz->pos p1) (xyz->pos p0)))])
         (rotate (rotate (prism-z center (p3d:dir-dx dir) (p3d:dir-dy dir) (p3d:dir-dist vec-dir) sides r g b angle-x 1.0 0.0 0.0) angle-y 0.0 1.0 0.0)  angle-z 0.0 0.0 1.0)
         ))
      )
  )
#;(define (prism-p p0 w h p1 sides r g b)
  (if (not (xyz? p1))
      (cylinder-z p0 h (/ p1 2 ))
      (let* ((vec-dir (p3d:pos- (xyz->pos p1) (xyz->pos p0)))
             (rot-dir (p3d:dir-normalize (p3d:dir-cross vec-dir p3d:+z)))
             (dir (p3d:dir w h (/ (p3d:dir-dist vec-dir) 2)))
             (center (p3d:pos-between (xyz->pos p0) (xyz->pos p1) 1/2))
             (angle (acos (/ (p3d:dir-dot vec-dir p3d:+z) (* (p3d:dir-dist vec-dir) (p3d:dir-dist p3d:+z))))))
        (match-let-values
         ([(yaw pit) (p3d:dir->angles (p3d:pos- (xyz->pos p1) (xyz->pos p0)))])
         (prism-z center (p3d:dir-dx dir) (p3d:dir-dy dir) (p3d:dir-dist vec-dir) sides r g b (- angle) (p3d:dir-dx rot-dir) (p3d:dir-dy rot-dir) (p3d:dir-dz rot-dir))
         ))
      )
  )
(define (prism-p p0 w h p1 sides r g b)
  (if (not (xyz? p1))
      (cylinder-z p0 h (/ p1 2 ))
      (let* ((vec-dir (p3d:pos- (xyz->pos p1) (xyz->pos p0)))
             (rot-dir (p3d:dir-normalize (p3d:dir-cross vec-dir p3d:+y)))
             (dir (p3d:dir w h (/ (p3d:dir-dist vec-dir) 2)))
             (center (p3d:pos-between (xyz->pos p0) (xyz->pos p1) 1/2))
             (angle-x (acos (/ (p3d:dir-dot (p3d:dir-project vec-dir p3d:+x) p3d:+z) (* (p3d:dir-dist (p3d:dir-project vec-dir p3d:+x)) (p3d:dir-dist p3d:+z)))))
             (angle-y (acos (/ (p3d:dir-dot (p3d:dir-project vec-dir p3d:+y) vec-dir) (* (p3d:dir-dist (p3d:dir-project vec-dir p3d:+y)) (p3d:dir-dist vec-dir)))))
             (angle-z (acos (/ (p3d:dir-dot (p3d:dir-project vec-dir p3d:+x+y) p3d:+z) (* (p3d:dir-dist (p3d:dir-project vec-dir p3d:+x+y)) (p3d:dir-dist p3d:+z))))))
        ;(displayln angle-x)
        ;(displayln angle-y)
        ;(displayln angle-z)
        ;(displayln "")
        (match-let-values
         ([(yaw pit) (p3d:dir->angles (p3d:pos- (xyz->pos p1) (xyz->pos p0)))])
         (rotate (rotate (prism-z center (p3d:dir-dx dir) (p3d:dir-dy dir) (p3d:dir-dist vec-dir) sides r g b angle-x 1.0 0.0 0.0) angle-y 0.0 1.0 0.0)  angle-z 0.0 0.0 1.0)
         ))
      )
  )

#;(define (cs-from-o-vz [o : Loc] [n : Vec])
  (let ((o (loc-in-world o))
        (n (vec-in-world n)))
    (let ((vx (vpol 1 (+ (sph-phi n) pi/2))))
      (let ((vy (unitize (v*v n vx))))
        (let ((vz (unitize n)))
          (cs-from-o-vx-vy-vz o vx vy vz))))))

(define (cs-from-o-vx-vy-vz o ux uy uz)
  (matrix [[(cx ux) (cx uy) (cx uz) (cx o)]
               [(cy ux) (cy uy) (cy uz) (cy o)]
               [(cz ux) (cz uy) (cz uz) (cz o)]
               [     0       0       0      1 ]]))

(define (vpol rho phi)
  (p3d:dir (* rho (cos phi))
           (* rho (sin phi))
           0.0))
(define (unitize v)
  (p3d:dir-normalize v))

(define (v*v v1 v2)
  (p3d:dir-cross v1 v2))

(define (cs-from-o-vz o n)
    (let ((vx (vpol 1 (+ (atan (p3d:dir-dy n) (p3d:dir-dx n)) pi/2))))
      (let ((vy (unitize (v*v n vx))))
        (let ((vz (unitize n)))
          (displayln vx)
          (displayln vy)
          (displayln vz)
          (cs-from-o-vx-vy-vz o (dir->xyz vx) (dir->xyz vy) (dir->xyz vz))))))

;(p3d:dir-dy rot-dir) (p3d:dir-dz rot-dir)

(define (right-cuboid p1 w h p2 r g b)
  (let* ([comp (distance p1 p2)]
        [center (pos->xyz (mid-point p1 p2))]
        [args (map exact->inexact (list (cx p1) (cy p1) (cz p1) (cx p2) (cy p2) (cz p2) w h comp 4.0 r g b))])
    ;(box-p center comp w h  0.0 0.0 0.0)
    (apply prismpts args)
    ;(prism-p p1 w h p2 4.0 r g b )
    )
  )

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

(define (malha-teste)
  (map-division (lambda (i j)                              
                  (xyz i (* j (sinusoide 0.15 1.75 0 i)) j))
                  ;(xyz i (* j (sinusoide 0.2 1.5 0 i)) (+ j (* 1.5 (sinusoide 0.3 1.5 0 i)))))
                -21 21 105
                0 7 55))



(init 100)
(itera-quadrangulos tijolo (malha-teste))
;(right-cuboid (xyz 0.0 0.0 0.0) 1.0 1.0 (xyz 2.0 0.0 0.0) 0.9 0.9 0.9)
;(right-cuboid (xyz 0.0 0.0 2.0) 1.0 1.0 (xyz 2.0 2.0 2.0) 0.9 0.9 0.9)
;(right-cuboid (xyz 0.0 0.0 4.0) 1.0 1.0 (xyz 0.0 2.0 4.0) 0.9 0.9 0.9)
;(right-cuboid (xyz 2.0 0.0 0.0) 1.0 1.0 (xyz 3.0 1.0 0.0) 0.9 0.9 0.9)
;(right-cuboid (xyz 2.0 (- 1.0) 0.0) 1.0 1.0 (xyz 2.5 1.0 0.0) 0.9 0.9 0.9)
;(prism-p (xyz (- 2.0) (- 1.0) 0.0) 2.0 1.0 (xyz (- 2.5) 1.0 0.0) 4.0 0.0 1.0 0.5)
;(prism-p (xyz (- 2.0) (- 1.0) 10.0) 2.0 1.0 (xyz (- 2.5) 1.0 10.0) 4.0 0.0 1.0 0.5)
(time 
  (begin 
    (send_data)
    (start)
    ))

(displayln "END")


