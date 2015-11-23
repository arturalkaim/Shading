#lang racket
(require ffi/unsafe
         ffi/unsafe/define
         math/flonum
         (except-in  "../base.rkt" sphere cylinder box)
         (prefix-in p3d: pict3d)
         racket/trace
         )

(define-ffi-definer define-test (ffi-lib "/Users/arturalkaim/Shaders/libShaders"))

(define-test start (_fun -> _int))
(define-test createPoints (_fun _int -> _int))
(define-test box (_fun _float _float _float _float _float _float -> _int))
(define-test cylinder (_fun _float _float _float _float _float -> _int))
(define-test sphere (_fun _float _float _float _float -> _int))
(define-test pyramid (_fun _float _float _float _float _float _float _float -> _int))
(define-test rotate (_fun _int _float _float _float _float -> _int))
(define-test move (_fun _int _float _float _float -> _int))
(define-test scale (_fun _int _float _float _float -> _int))
(define-test init (_fun _int -> _int))
(define-test city (_fun _int -> _void))


(define (box-p p l w h)
  (box (cx p) (cy p) (cz p) l w h))
(define (cube-p p l)
  (box (cx p) (cy p) (cz p) l l l))
(define (sphere-p p r)
  (sphere (cx p) (cy p) (cz p) r))
(define (cylinder-z p l h)
  (cylinder (p3d:pos-x p) (p3d:pos-y p) (p3d:pos-z p) l h))

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
         (rotate (cylinder-z center (p3d:dir-dx dir) (p3d:dir-dz dir)) angle (p3d:dir-dx rot-dir) (p3d:dir-dy rot-dir) (p3d:dir-dz rot-dir))
         ))
      )
  )

(trace cylinder-p)
#;
(define (building x y z w l h)
  (let ([h1 (* 0.7 h)]
        [h2 (* 0.4 h)])
    (begin
      (box x y h1 w l h1)
      (cylinder x y (+ (* h1 2) h2) (* 0.7 w) h2)
      )
    )
  )


(init 1000)

#;
(let ([grid-size 30.0])
  (for* ([xi  (in-range (- grid-size) grid-size 0.3)]
         [yi  (in-range (- grid-size) grid-size 0.3)])
    (building xi yi 0.0 0.1 0.1 (random))))


;(scale (rotate (move (cylinder-p (xyz 0.0 0.0 0.0) 0.3 (xyz 1.0 1.0 1.0)) 0.0 1.0 0.0) 10.0 1.0 0.0 0.0) 1.0 3.0 1.0)

;(city 200)
#;
(begin
  (cylinder -8.0 -8.0 0.0 1.0 0.4)
  (cylinder -4.0 -4.0 0.0 1.0 0.4)
  (cylinder 0.0 0.0 0.0 1.0 0.4)
  (cylinder 4.0 4.0 0.0 1.0 0.4)
  (cylinder 8.0 8.0 0.0 1.0 0.4))
;(box -0.4 0.4 0.2 0.2)
;(box 1.0 0.0 0.2 0.2)
;(box 0.2 0.4 0.2 0.2)
;(box -0.4 0.4 0.2 0.2)


;(start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define truss-node-radius (make-parameter 0.05))

(define (no-trelica p)
  (cube-p p (truss-node-radius)))

(define truss-radius-bar (make-parameter 0.01))

(define (barra-trelica p0 p1)
  (when (not (=c? p0 p1))
    ; (empty-shape)
    (cylinder-p p0 (truss-radius-bar) p1)))

(define (nos-trelica ps)
  (map no-trelica ps))

(define (barras-trelica ps qs)
  (for/list ((p (in-list ps))
             (q (in-list qs)))
    (barra-trelica p q)))

(define (spacial-truss curvas)
  (let ((as (car curvas))
        (bs (cadr curvas))
        (cs (caddr curvas)))
    (nos-trelica as)
    (nos-trelica bs)
    (barras-trelica as cs)
    (barras-trelica bs as)
    (barras-trelica bs cs)
    (barras-trelica bs (cdr as))
    (barras-trelica bs (cdr cs))
    (barras-trelica (cdr as) as)
    (barras-trelica (cdr bs) bs)
    (if (null? (cdddr curvas))
        (begin
          (nos-trelica cs)
          (barras-trelica (cdr cs) cs))
        (begin
          (barras-trelica bs (cadddr curvas))
          (spacial-truss (cddr curvas))))))

(define (media-pontos p0 p1)
  (xyz (/ (+ (cx p0) (cx p1)) 2)
       (/ (+ (cy p0) (cy p1)) 2)
       (/ (+ (cz p0) (cz p1)) 2)))

(define (centro-quadrangulo p0 p1 p2 p3)
  (media-pontos
   (media-pontos p0 p2)
   (media-pontos p1 p3)))

(define (normal-poligono pts)
  (vector-normalizado
   (produtos-cruzados
    (append pts (list (car pts))))))

(define (produtos-cruzados pts)
  (if (null? (cdr pts))
      (xyz 0 0 0)
      (+c (produto-cruzado (car pts) (cadr pts))
          (produtos-cruzados (cdr pts)))))

(define (produto-cruzado p0 p1)
  (xyz (* (- (cy p0) (cy p1)) (+ (cz p0) (cz p1)))
       (* (- (cz p0) (cz p1)) (+ (cx p0) (cx p1)))
       (* (- (cx p0) (cx p1)) (+ (cy p0) (cy p1)))))

(define (vector-normalizado v)
  (let ((l (sqrt (+ (sqr (cx v))
                    (sqr (cy v))
                    (sqr (cz v))))))
    (xyz (/ (cx v) l)
         (/ (cy v) l)
         (/ (cz v) l))))

(define (normal-quadrangulo p0 p1 p2 p3)
  (normal-poligono (list p0 p1 p2 p3)))


(define (quadrangular-pyramid-vertex p0 p1 p2 p3)
  (let ((h (/ (+ (distance p0 p1)
                 (distance p1 p2)
                 (distance p2 p3)
                 (distance p3 p0))
              4.0
              (sqrt 2))))
    (+c (centro-quadrangulo p0 p1 p2 p3)
        (*c (normal-quadrangulo p0 p1 p2 p3)
            h))))

(define (insert-pyramid-vertex ptss)
  (if (null? (cdr ptss))
      ptss
      (cons
       (car ptss)
       (cons (insert-pyramid-vertex-2 (car ptss) (cadr ptss))
             (insert-pyramid-vertex (cdr ptss))))))

(define (insert-pyramid-vertex-2 pts0 pts1)
  (cons (quadrangular-pyramid-vertex (car pts0) (car pts1) (cadr pts1) (cadr pts0))
        (if (null? (cddr pts0))
            (list)
            (insert-pyramid-vertex-2 (cdr pts0) (cdr pts1)))))

(define (render-truss matrix)
  (let* ((p0 (caar matrix))
         (p1 (caadr matrix))
         (p2 (cadadr matrix))
         (p3 (cadar matrix))
         (d (min (distance p0 p1) (distance p0 p3))))
    (parameterize ((truss-node-radius (/ d 9.0))
                   (truss-radius-bar (/ d 19.0)))
      (spacial-truss
       (insert-pyramid-vertex
        matrix)))))

(define (sin-u*v n)
  (map-division
   (lambda (u v)
     (xyz (* u 10)
          (* v 10)
          (* 4 (sin (* u v)))))
   (* -1 pi) (* 1 pi) n
   (* -1 pi) (* 1 pi) n))

(render-truss (sin-u*v 90))
(displayln "END")

;(display (start))
#;
(p3d:rotate (p3d:dir 1.0 1.0 0.0) (/ pi 4))
#;(p3d:rotate (p3d:dir 1.0 0.0 0.0) (/ pi 4))
#;(p3d:linear-compose (p3d:rotate (p3d:dir 0.0 1.0 0.0) (- (/ pi 4))) (p3d:rotate (p3d:dir 1.0 0.0 0.0) (/ pi 4)))
#;(p3d:combine
   (p3d:cylinder p3d:origin (p3d:dir 0.5 0.5 2.0))
   (p3d:transform (p3d:cylinder p3d:origin p3d:+x+y+z) (p3d:rotate (p3d:dir 1.0 1.0 0.0) (/ pi 4))))
#;(p3d:linear-compose (p3d:rotate (p3d:dir 0.0 0.0 1.0) (/ pi 4)) (p3d:rotate (p3d:dir 1.0 0.0 0.0) (/ pi 4)))
#;(p3d:pos- (p3d:pos 2.0 2.0 2.0) p3d:origin)

#;(p3d:dir-project (p3d:pos- (p3d:pos 2.0 2.0 2.0) p3d:origin) (p3d:pos- (p3d:pos 1.0 0.0 1.0) p3d:origin))
#;(p3d:dir-project (p3d:pos- (p3d:pos 2.0 2.0 2.0) p3d:origin) (p3d:pos- (p3d:pos 1.0 1.0 0.0) p3d:origin))

#;(p3d:pos- (p3d:pos 2.0 0.0 2.0) p3d:origin)
