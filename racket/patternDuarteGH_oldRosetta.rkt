#lang racket
(require (planet aml/rosetta))
(require racket/date)
(backend autocad)

(define (itera-pts f ptss)
  (for/list ((pts0 ptss))
    (for/list ((p0 pts0))
      (f p0))))

(define (itera-quads f ptss)
  (union
   (append*
    (for/list ((pts0 ptss)
               (pts1 (cdr ptss)))
      (for/list ((p0 pts0)
                 (p1 pts1)
                 (p2 (cdr pts1))
                 (p3 (cdr pts0)))
        (f p0 p1 p2 p3))))))

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

(define (union-mirror shape p n)
  (union
   shape
   (mirror shape p n)))

;Angles with PI/10
(define 6pi/10 (* 6 (/ pi 10)))
(define 7pi/10 (* 7 (/ pi 10)))
(define 8pi/10 (* 8 (/ pi 10)))
(define 9pi/10 (* 9 (/ pi 10)))
;Angles with PI/20
(define 11pi/20 (* 11 (/ pi 20)))
(define 13pi/20 (* 13 (/ pi 20)))
(define 15pi/20 (* 15 (/ pi 20)))
(define 17pi/20 (* 17 (/ pi 20)))
(define 19pi/20 (* 19 (/ pi 20)))
;Angles with -PI/8
(define pi/8 (* -1 (/ pi 8)))
(define 2pi/8 (* -2 (/ pi 8)))
(define 3pi/8 (* -3 (/ pi 8)))
(define 4pi/8 (* -1 (/ pi 2)))
;Angles with -PI/16
(define pi/16 (* -1 (/ pi 16)))
(define 3pi/16 (* -3 (/ pi 16)))
(define 5pi/16 (* -5 (/ pi 16)))
(define 7pi/16 (* -7 (/ pi 16)))
;Factors
(define f 0.95) ;Big-star
(define ff 0.98) ;Small-star

(define (pattern p0 p1 p2 p3)
  (let* ((p (centro-quadrangulo p0 p1 p2 p3))
         (dp (distance (media-pontos p0 p3) p))
         ;BIGGER STAR P1
         ;Radius Star-Interior
         (r-int (* dp 0.25))
         (r-intm (* dp 0.25 f))
         (r-inte (* dp 0.5))
         ;Points Star-int
         (p11 (+pol p r-intm 6pi/10))
         (p12 (+pol p r-intm 8pi/10))
         (p13 (+pol p r-intm pi))
         (p21 (+pol p r-int pi/2))
         (p22 (+pol p r-int 7pi/10))
         (p23 (+pol p r-int 9pi/10))
         (p31 (+pol p r-inte 6pi/10))
         (p32 (+pol p r-inte 8pi/10))
         (p33 (+pol p r-inte pi))
         ;Radius star-middle
         (r-mid (* dp 0.75))
         (r-midi (* dp 0.75 f))
         (r-mide (* dp 0.875))
         (p41 (+pol p r-midi 11pi/20))
         (p42 (+pol p r-midi 13pi/20))
         (p43 (+pol p r-midi 15pi/20))
         (p44 (+pol p r-midi 17pi/20))
         (p45 (+pol p r-midi 19pi/20))
         (p51 (+pol p r-mid pi/2))
         (p52 (+pol p r-mid 7pi/10))
         (p53 (+pol p r-mid 9pi/10))
         (p61 (+pol p r-mide 6pi/10))
         (p62 (+pol p r-mide 8pi/10))
         (p63 (+pol p r-mide pi))
         ;Radious star-exterior
         (r-ext (* dp 1.2))
         (p71 (+pol p r-ext pi/2))
         (p72 (+pol p r-ext 7pi/10))
         (p73 (+pol p r-ext 9pi/10))
         ;SMALLER STAR P3
         ;Radius star-Int
         (rr-inti (* 0.125 dp))
         (rr-int (* 0.15 dp))
         (pa1 (+pol p3 rr-inti 0))
         (pa2 (+pol p3 rr-inti 2pi/8))
         (pa3 (+pol p3 rr-inti 4pi/8))
         (pb1 (+pol p3 rr-int pi/8))
         (pb2 (+pol p3 rr-int 3pi/8))
         ;Radius star-mid
         (rr-midi (* 0.33 dp))
         (rr-mid (* 0.5 dp ff))
         (rr-mide (* 0.5 dp))
         (pc1 (+pol p3 rr-midi 0))
         (pc2 (+pol p3 rr-midi 2pi/8))
         (pc3 (+pol p3 rr-midi 4pi/8))
         (pd1 (+pol p3 rr-mid pi/16))
         (pd2 (+pol p3 rr-mid 3pi/16))
         (pd3 (+pol p3 rr-mid 5pi/16))
         (pd4 (+pol p3 rr-mid 7pi/16))
         (pe1 (+pol p3 rr-mide pi/8))
         (pe2 (+pol p3 rr-mide 3pi/8))
         ;Radius star-points
         (rr-exti (* 0.6 dp))
         (rr-ext (* 0.9 dp))
         (pf1 (+pol p3 rr-exti 0))
         (pf2 (+pol p3 rr-exti 2pi/8))
         (pf3 (+pol p3 rr-exti 4pi/8))
         (pg1 (+pol p3 rr-ext pi/8))
         (pg2 (+pol p3 rr-ext 3pi/8))
         ;triangle
         (pt1 (+pol p3 (* 0.7 dp) pi/8))
         (pt2 (ponto-intermedio (media-pontos p3 p2) p 0.15))
         (pt3 (ponto-intermedio (media-pontos p3 p2) p 0.45)))
    (union-mirror
     (union-mirror
      (union
       (with-current-layer "int" (surface (list (line p13 p p21 p11 p22 p12 p23 p13))))
       (with-current-layer "midd"
                           (union
                            (surface (list (line p21 p31 p22 p11 p21)))
                            (surface (list (line p22 p32 p23 p12 p22)))
                            (surface (list (line p23 p33 p13 p23)))))
       ;Big-star-middle
       (with-current-layer "midd-out"
                           (union
                            (surface (list (line p31 p41 p51 p61 p52 p42 p31)))
                            (surface (list (line p32 p43 p52 p62 p53 p44 p32)))
                            (surface (list (line p33 p45 p53 p63 p33)))))
       ;Big-star-points
       (with-current-layer "points"
                           (union
                            (surface (list (line p51 p71 p61 p51)))
                            (surface (list (line p52 p61 p72 p62 p52)))
                            (surface (list (line p53 p62 p73 p63 p53)))))
       ;Small-star
       (with-current-layer "int2" (surface (list (line p3 pa1 pb1 pa2 pb2 pa3 p3))))
       (with-current-layer "midd2"
                           (union
                            (surface (list (line pa1 pb1 pc1 pa1)))
                            (surface (list (line pa2 pb1 pc2 pb2 pa2)))
                            (surface (list (line pa3 pb2 pc3 pa3)))))
       ;small-star-middle
       (with-current-layer "midd-out2"
                           (union
                            (surface (list (line pc1 pd1 pe1 pf1 pc1)))
                            (surface (list (line pc2 pd2 pe1 pf2 pe2 pd3 pc2)))
                            (surface (list (line pc3 pd4 pe2 pf3 pc3)))))
       ;small-star-points
       (with-current-layer "points2"
                           (union
                            (surface (list (line pf1 pg1 pf2 pe1 pf1)))
                            (surface (list (line pf2 pg2 pf3 pe2 pf2)))))
       ;triangle
       (with-current-layer "triangle" (surface (list (line pt1 pt2 pt3 pt1)))))
      p (uy))
     p (ux))))

(define (run n s)
  (begin (itera-quads pattern (map-division (lambda (i j)
                                       (xyz i j 0))
                                     (- n) n s
                                     (- n) n (/ s 2)))
         ))



;run n s
;Autocad
;10 2 cpu time: 188 real time: 339 gc time: 47 cpu time: 203 real time: 360 gc time: 0
;10 4 cpu time: 953 real time: 1656 gc time: 110 cpu time: 515 real time: 1119 gc time: 47 cpu time: 422 real time: 1073 gc time: 32
;10 8  cpu time: 2563 real time: 4982 gc time: 203
;10 10 cpu time: 2000 real time: 6798 gc time: 172 cpu time: 2094 real time: 6984 gc time: 189 cpu time: 2516 real time: 9081 gc time: 204
;20 10 cpu time: 2234 real time: 7201 gc time: 202
;10 14 cpu time: 3750 real time: 13758 gc time: 345
;20 16 cpu time: 7813 real time: 20719 gc time: 469 cpu time: 8485 real time: 27496 gc time: 1609 cpu time: 9593 real time: 25438 gc time: 642
;Rhino5
;10 2 cpu time: 1312 real time: 4328 gc time: 531 cpu time: 203 real time: 3458 gc time: 0
;10 4 cpu time: 1250 real time: 14954 gc time: 62
;10 6 cpu time: 1375 real time: 41561 gc time: 77
;10 8 cpu time: 2437 real time: 101314 gc time: 78 cpu time: 2766 real time: 107745 gc time: 142
;10 10 cpu time: 12625 real time: 225979 gc time: 1248
;20 10 227047  222080
;10 14 740052 (30729 minimizado)
;20 20 cpu time: 22141 real time: 1045044 gc time: 544
(define (run10) (run 10 10))
;(run 10 14)

(define out (open-output-file (format "r-times-~a-~a.tms" "autocad" "padrao") #:mode 'text #:exists 'append))
(define (test)
  (let ([sizes (reverse (list 10 20 30))]
        [divs (reverse (list 4 6 8 10 12 14 16 18 20))])
    (for ((s sizes))
      (for ((d divs))
        (define input (list s d))
        (define-values (res cpu real gc)(time-apply run input))
        (writeln (format "~a ~a" s d) out)
        (writeln real out)))))
(current-date)
;(test)
(current-date)
#;(let ([id (thread run10)])
    (sleep 2)
    (kill-thread id)
    id)