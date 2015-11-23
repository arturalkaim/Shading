#lang racket
(require
  (prefix-in p3d: pict3d))


(provide pi/2)
(define pi/2 (/ pi 2))

(provide random-range random)
(define (random-range x0 x1)
  (+ x0 (random (- x1 x0))))

(define ultimo-aleatorio-gerado 12345)

(define (set-ultimo-aleatorio-gerado! v)
  (set! ultimo-aleatorio-gerado v))

(define (aleatorio)
  (set! ultimo-aleatorio-gerado
        (proximo-aleatorio ultimo-aleatorio-gerado))
  ultimo-aleatorio-gerado)

(define (proximo-aleatorio ultimo-aleatorio)
  (let ((teste (- (* 16807 (remainder ultimo-aleatorio 127773))
                  (* 2836  (quotient ultimo-aleatorio 127773)))))
    (if (> teste 0)
        (if (> teste 2147483647)
            (- teste 2147483647)
            teste)
        (+ teste 2147483647))))

(define (random x)
  (if (inexact? x)
      (* x (/ (aleatorio) 2147483647.0))
      (remainder (aleatorio) x)))

(provide pic-list)
(define pic-list '())


(provide xyz xy xyz?)
(struct xyz (x y z) #:mutable #:transparent)
(define (xy x y) (xyz x y 0.0))
(define (xz x z) (xyz x 0.0 z))
(define (yz y z) (xyz 0.0 y z))
(provide cx cy cz)
(define cx xyz-x)
(define cy xyz-y)
(define cz xyz-z)

(provide +xyz +xy -xy x+ y+ z+ +xz)
(define (+xyz p x y z)
  (xyz (+ (cx p) x) (+ (cy p) y) (+ (cz p) z)))

(define (+xy p x y)
  (xyz (+ (cx p) x) (+ (cy p) y) (cz p)))

(define (-xy p x y)
  (xyz (- (cx p) x) (- (cy p) y) (cz p)))

(define (+xz p dx dz)
  (xyz (+ (cx p) dx) (cy p) (+ (cz p) dz)))

(define (x+ p x)
  (xyz (+ (cx p) x) (cy p) (cz p)))

(define (y+ p y)
  (xyz (cx p) (+ (cy p) y) (cz p)))

(define (z+ p z)
  (xyz (cx p) (cy p) (+ (cz p) z)))

(provide =c? +c /c -c *c)
(define (=c? c1 c2)
  (or (eq? c1 c2)
      (and (eq? (cx c1) (cx c2))
           (eq? (cy c1) (cy c2))
           (eq? (cz c1) (cz c2)))))

(define (+c c1 c2)
  (xyz (+ (cx c1) (cx c2))
       (+ (cy c1) (cy c2))
       (+ (cz c1) (cz c2))))

(define (/c c1 v)
  (xyz (/ (cx c1) v)
       (/ (cy c1) v)
       (/ (cz c1) v)))

(define (-c c1 c2)
  (xyz (- (cx c1) (cx c2))
       (- (cy c1) (cy c2))
       (- (cz c1) (cz c2))))

(define (*c c1 p)
  (xyz (* (cx c1) p)
       (* (cy c1) p)
       (* (cz c1) p)))


(provide pos->xyz xyz->pos xyz->dir pos->dir distance mid-point norm-c dir->xyz)
(define (pos->xyz p)
  (xyz (p3d:pos-x p) (p3d:pos-y p) (p3d:pos-z p)))
(define (xyz->pos p)
  (p3d:pos (cx p) (cy p) (cz p)))
(define (xyz->dir p)
  (p3d:dir (cx p) (cy p) (cz p)))
(define (pos->dir p)
  (p3d:dir (p3d:pos-x p) (p3d:pos-y p) (p3d:pos-z p)))
(define (distance p1 p2)
  (p3d:pos-dist (xyz->pos p1) (xyz->pos p2)))
(define (mid-point p1 p2)
  (p3d:pos-between (xyz->pos p1) (xyz->pos p2) 1/2))
(define (norm-c p1)
  (let ((p  (p3d:dir-normalize (xyz->dir p1) )))
    (xyz (p3d:dir-dx p) (p3d:dir-dy p) (p3d:dir-dz p))))

(define (dir->xyz p)
  (xyz (p3d:dir-dx p) (p3d:dir-dy p) (p3d:dir-dz p)))




(provide cyl +pol +cyl)
(define (cyl rho phi z)
  (xyz (* rho (cos phi))
       (* rho (sin phi))
       z))

(define (+cyl p rho phi z)
  (+c p (cyl rho phi z)))

(define (cyl-rho p)
  (let ((x (xyz-x p))
        (y (xyz-y p)))
    (sqrt (+ (* x x) (* y y)))))

(define (cyl-phi c)
  (sph-phi c))

(define (cyl-z c)
  (xyz-z c))

(define (pol rho phi)
  (cyl rho phi 0))

(define (+pol p rho phi)
  (+cyl p rho phi 0))

(define pol-rho cyl-rho)

(define pol-phi cyl-phi)

(define (sph rho phi psi)
  (let ((sin-psi (sin psi)))
    (xyz (* rho (cos phi) sin-psi)
         (* rho (sin phi) sin-psi)
         (* rho (cos psi)))))

(define (+sph p rho phi psi)
  (+c p (sph rho phi psi)))

(define (sph-rho p)
  (let ((x (xyz-x p))
        (y (xyz-y p))
        (z (xyz-z p)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

(define (sph-phi p)
  (let ((x (xyz-x p))
        (y (xyz-y p)))
    (if (= 0 x y)
        0
        (atan (xyz-y p) (xyz-x p)))))

(define (sph-psi p)
  (let ((x (xyz-x p))
        (y (xyz-y p))
        (z (xyz-z p)))
    (if (= 0 x y z)
        0
        (atan (sqrt (+ (* x x) (* y y)))
              z))))

(define (+rho c rho)
  (cyl (+ (cyl-rho c) rho) (cyl-phi c) (cyl-z c)))

(define (+phi c phi)
  (cyl (cyl-rho c) (+ (cyl-phi c) phi) (cyl-z c)))

(define (+r c r)
  (sph (+ (sph-rho c) r) (sph-phi c) (sph-psi c)))

(define (+psi c psi)
  (sph (sph-rho c) (sph-phi c) (+ (sph-psi c) psi)))

(provide box right-cuboid cylinder sphere)
(define (box p l w h)
  ;(begin (displayln "box")
  (set! pic-list (list pic-list (p3d:rectangle (xyz->pos p) (p3d:pos (+ (cx p) l) (+ (cy p) w) (+ (cz p) h))))));)


(define (right-cuboid [p (xyz 0 0 0)] [l 1] [w 1] [h 1])
  (set! pic-list (list pic-list (p3d:rectangle (xyz->pos (z+ p (/ h 2))) (p3d:dir (/ l 2) (/ w 2) (/ h 2))))))





(define (cylinder p0 h p1)
  ;(begin (displayln "cylinder")
  (if (not (xyz? p1))
      (set! pic-list  (list pic-list (p3d:cylinder (p3d:pos (cx p0) (cy p0) (+ (/ p1 2 ) (cz p0))) (p3d:dir h h (/ p1 2 )))))
      (set! pic-list  (list pic-list 
                            (let ((dir (p3d:dir h h (/ (p3d:dir-dist (p3d:pos- (xyz->pos p1) (xyz->pos p0))) 2)))
                                  (center (p3d:pos-between (xyz->pos p0) (xyz->pos p1) 1/2)))
                              (match-let-values
                               ([(yaw pit) (p3d:dir->angles (p3d:pos- (xyz->pos p1) (xyz->pos p0)))])
                               (p3d:move
                                (p3d:rotate-z (p3d:rotate-y (p3d:cylinder p3d:origin dir) (- 90 pit) ) yaw) (pos->dir center))
                               ))
                            ))
      )
  )

(define (sphere p r)
  (set! pic-list  (list pic-list (p3d:sphere (p3d:pos (cx p) (cy p) (cz p)) r))))



(provide mod map-division)
(define (mod x y)
  (- x (* (floor (/ x y)) y)))


(define-sequence-syntax division
  (lambda () #'division/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(v) (clause from to elems)]
       #'[(v)
          (clause from to elems #t)]]
      [[(v) (_ from to elems last?)]
       #`[(v)
          (:do-in
           ([(a) from] [(b) to] [(n) elems]
                       #,@(case (syntax-e #'last?)
                            ((#t #f) #'())
                            (else #'([(pred) (if last? <= <)]))))
           (unless (exact-positive-integer? n)
             (raise-type-error 'division "exact non-negative integer" n))
           ([i 0])
           (#,(case (syntax-e #'last?)
                ((#t) #'<=)
                ((#f) #'<)
                (else #'pred))
            i n)
           ([(v) (+ a (/ (* i (- b a)) n))])
           #true
           #true
           ((+ i 1)))]])))

(define (division/proc a b n [last? #t])
  (if last?
      (for/list ([t (division a b n #t)])
        t)
      (for/list ([t (division a b n #f)])
        t)))

(define map-division
  (case-lambda
    ((f t0 t1 n)
     (for/list ((t (division t0 t1 n)))
       (f t)))
    ((f t0 t1 n last?)
     (for/list ((t (division t0 t1 n last?)))
       (f t)))
    ((f u0 u1 nu v0 v1 nv)
     (for/list ((u (division u0 u1 nu)))
       (for/list ((v (division v0 v1 nv)))
         (f u v))))
    ((f u0 u1 nu lastu? v0 v1 nv)
     (for/list ((u (division u0 u1 nu lastu?)))
       (for/list ((v (division v0 v1 nv)))
         (f u v))))
    ((f u0 u1 nu lastu? v0 v1 nv lastv?)
     (for/list ((u (division u0 u1 nu lastu?)))
       (for/list ((v (division v0 v1 nv lastv?)))
         (f u v))))))



;;draw receives a list of points (xyz) and returns a list 
;;of pict2d objects that combined represent a surface mesh
(provide draw)
(define (draw lst [color (p3d:rgba "Azure")] #:smoth [s? #t] #:back [b? #t])
  (let ([lgt1 (length lst)]
        [lgt2 (length (first lst))])
    (define (get lsta x y)
      (list-ref (list-ref lsta x) y))
    #;  
    (define (norm i j) 
      (p3d:dir-cross (p3d:pos- (xyz->pos (get lst i j)) (xyz->pos (get lst i (+ j 1)))) (p3d:pos- (xyz->pos (get lst i j)) (xyz->pos (get lst (+ i 1) j)))))
    
    (define (norm i j) 
      (p3d:dir-cross (xyz->dir (-c (get lst i j) (get lst i  (modulo (+ j 1) lgt2)))) (xyz->dir (-c (get lst i j) (get lst (modulo (+ i 1) lgt1) j)))))
    
    (if b? 
        (if s? (for*/list
                   ([i (range (- lgt1 1))]
                    [j (range (- lgt2 1))])
                 (let ([vij   (p3d:vertex  (xyz->pos (get lst i j)) #:normal (norm i j) #:color color)] ;
                       [v+ij  (p3d:vertex  (xyz->pos (get lst (modulo (+ i 1) lgt1)  (modulo (+ j 0) lgt2))) #:normal (norm (+ i 1) j) #:color color)]
                       [v+i+j (p3d:vertex  (xyz->pos (get lst (modulo (+ i 1) lgt1)  (modulo (+ j 1) lgt2))) #:normal (norm (+ i 1) (+ j 1)) #:color color)] ; 
                       [vi+j  (p3d:vertex  (xyz->pos (get lst (modulo (+ i 0) lgt1)  (modulo (+ j 1) lgt2))) #:normal (norm i (+ j 1)) #:color color)]
                       [vij2   (p3d:vertex  (xyz->pos (get lst i j)) #:normal (p3d:dir-negate (norm i j)) #:color color)] ;
                       [v+ij2  (p3d:vertex  (xyz->pos (get lst (modulo (+ i 1) lgt1)  (modulo (+ j 0) lgt2))) #:normal (p3d:dir-negate (norm (+ i 1) j)) #:color color)]
                       [v+i+j2 (p3d:vertex  (xyz->pos (get lst (modulo (+ i 1) lgt1)  (modulo (+ j 1) lgt2))) #:normal (p3d:dir-negate (norm (+ i 1) (+ j 1))) #:color color)] ; 
                       [vi+j2  (p3d:vertex  (xyz->pos (get lst (modulo (+ i 0) lgt1)  (modulo (+ j 1) lgt2))) #:normal (p3d:dir-negate (norm i (+ j 1))) #:color color)])
                   
                   (list (p3d:freeze (p3d:quad vij 
                                               v+ij  
                                               v+i+j 
                                               vi+j
                                               #:back? #f
                                               ))
                         (p3d:freeze (p3d:quad vij2 
                                               v+ij2  
                                               v+i+j2 
                                               vi+j2
                                               #:back? #t
                                               ))
                         ))
                 )
            (for*/list
                ([i (range (- lgt1 1))]
                 [j (range (- lgt2 1))])
              (let ([p  (xyz->pos (get lst i j))]
                    [p+ij  (xyz->pos (get lst (modulo (+ i 1) lgt1)  (modulo (+ j 0) lgt2)))]
                    [p+i+j (xyz->pos (get lst (modulo (+ i 1) lgt1)  (modulo (+ j 1) lgt2)))]
                    [pi+j  (xyz->pos (get lst (modulo (+ i 0) lgt1)  (modulo (+ j 1) lgt2)))])
                
                (list (p3d:freeze (p3d:quad p 
                                            p+ij  
                                            p+i+j 
                                            pi+j
                                            #:back? #f
                                            ))
                      (p3d:freeze (p3d:quad p 
                                            p+ij  
                                            p+i+j 
                                            pi+j
                                            #:back? b?
                                            ))
                      ))
              ))
        (if s? (for*/list
                   ([i (range (- lgt1 1))]
                    [j (range (- lgt2 1))])
                 (let ([vij   (p3d:vertex  (xyz->pos (get lst i j)) #:normal (norm i j) #:color color)] ;
                       [v+ij  (p3d:vertex  (xyz->pos (get lst (modulo (+ i 1) lgt1)  (modulo (+ j 0) lgt2))) #:normal (norm (+ i 1) j) #:color color)]
                       [v+i+j (p3d:vertex  (xyz->pos (get lst (modulo (+ i 1) lgt1)  (modulo (+ j 1) lgt2))) #:normal (norm (+ i 1) (+ j 1)) #:color color)] ; 
                       [vi+j  (p3d:vertex  (xyz->pos (get lst (modulo (+ i 0) lgt1)  (modulo (+ j 1) lgt2))) #:normal (norm i (+ j 1)) #:color color)]
                       [vij2   (p3d:vertex  (xyz->pos (get lst i j)) #:normal (p3d:dir-negate (norm i j)) #:color color)] ;
                       [v+ij2  (p3d:vertex  (xyz->pos (get lst (modulo (+ i 1) lgt1)  (modulo (+ j 0) lgt2))) #:normal (p3d:dir-negate (norm (+ i 1) j)) #:color color)]
                       [v+i+j2 (p3d:vertex  (xyz->pos (get lst (modulo (+ i 1) lgt1)  (modulo (+ j 1) lgt2))) #:normal (p3d:dir-negate (norm (+ i 1) (+ j 1))) #:color color)] ; 
                       [vi+j2  (p3d:vertex  (xyz->pos (get lst (modulo (+ i 0) lgt1)  (modulo (+ j 1) lgt2))) #:normal (p3d:dir-negate (norm i (+ j 1))) #:color color)])
                   
                   (p3d:freeze (p3d:quad vij 
                                         v+ij  
                                         v+i+j 
                                         vi+j
                                         #:back? #f
                                         ))
                   )
                 )
            (for*/list
                ([i (range (- lgt1 1))]
                 [j (range (- lgt2 1))])
              (let ([p  (xyz->pos (get lst i j))]
                    [p+ij  (xyz->pos (get lst (modulo (+ i 1) lgt1)  (modulo (+ j 0) lgt2)))]
                    [p+i+j (xyz->pos (get lst (modulo (+ i 1) lgt1)  (modulo (+ j 1) lgt2)))]
                    [pi+j  (xyz->pos (get lst (modulo (+ i 0) lgt1)  (modulo (+ j 1) lgt2)))])
                
                (p3d:freeze (p3d:quad p 
                                      p+ij  
                                      p+i+j 
                                      pi+j
                                      #:back? #f
                                      ))
                
                )
              )
            )
        )
    ))