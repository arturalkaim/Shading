#lang racket
(require ffi/unsafe
         ffi/unsafe/define
         math/flonum
         math/matrix
         "sliders.rkt"
         "base.rkt"
         (prefix-in p3d: pict3d)
         )

(init 10)

;(line (list (u0) (+xy (u0) 12.0 12.0) (+xy (u0) 16.0 2.0) (+xy (u0) 16.0 5.0) (+xy (u0) 12.0 15.0) ))

;(line (list (xyz 12.0 12.0 2.0) (xyz 16.0 2.0 2.0)))

(polygon-line (list (+z (u0) 2.0) (xyz 12.0 0.0 2.0) (xyz 15.0 6.0 2.0) (xyz 12.0 12.0 2.0) (xyz 0.0 12.0 2.0) (+z (u0) 2.0)))

;(point (u0) )
;(point (+xy (u0) 12.0 12.0) )
;(point (+xy (u0) 16.0 2.0) )

(polygon (list (u0) (xyz 12.0 0.0 0.0) (xyz 15.0 6.0 0.0) (xyz 12.0 12.0 0.0) (xyz 0.0 12.0 0.0) (u0)) 1.0 0.0 0.0)


;(sphere (xyz 12.0 12.0 2.0) 2.0)

(time
 (begin
   (send_data)
   (thread while)))

(read)