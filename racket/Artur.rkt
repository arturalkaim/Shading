#lang racket
(require (planet aml/rosetta:1:=48))
(backend rhino5)
(render-dir "C:\\Users\\Inês\\Pictures\\renders")
(render-size 1042 768)

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
  (let ((c (distance p0 p1)) 
        (h (/ (distance p0 p3) 2))
        (l (* 0.6 (distance p0 p1)))
        (n (normal-quadrangulo p0 p1 p2 p3))
        (p (centro-quadrangulo p0 p1 p2 p3)))
    (let ((p01 (media-pontos p0 p))
          (p02 (media-pontos p p2)))
      (let ((pp1 (+c p01 (*c n l)))
            (pp2 (+c p02 (*c n l))))
        (union
         (sweep (line p01 pp1) (surface-rectangle p0 c h))
         (sweep (line p02 pp2) (surface-rectangle p0 c h)))))))

(define (malha-teste)
  (map-division (lambda (i j)                              
                  (xyz i (* j (sinusoide 0.15 1.75 0 i)) j))
                  ;(xyz i (* j (sinusoide 0.2 1.5 0 i)) (+ j (* 1.5 (sinusoide 0.3 1.5 0 i)))))
                0 21 105
                0 7 55))


(itera-quadrangulos tijolo (malha-teste))




