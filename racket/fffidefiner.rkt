#lang racket
(require ffi/unsafe
         ffi/unsafe/define)
(require (for-syntax racket/match))

(define shading-lib-path (build-path "C:\\Users\\DEMO\\Documents\\Visual Studio 2015\\Projects\\shading\\x64\\Debug\\shading"))
;(define shading-lib-path (build-path "shading"))
(displayln shading-lib-path)
(define shading-lib (ffi-lib shading-lib-path #:fail (λ () (displayln "FAIL!!!"))))
(define-ffi-definer define-f-function shading-lib)

(provide defffi)



(define-syntax (defffi stx)
  (define (ffi-type<-str-type type)
    (case (string->symbol type)
      ((int) #'_int)
      ((float) #'_float)
      #;((char) #'_char)
      (else (error "Unknown type" type))))
  (syntax-case stx ()
    [(def str)
     (match (regexp-match #rx" *(.+) +(.+) *\\( *(.*) *\\)" (syntax->datum #'str))
       ((list _ return-type-str func-str params-str)
        (let ((func (datum->syntax stx (string->symbol func-str)))
              (return-type (ffi-type<-str-type return-type-str))
              (params-strs (regexp-split #rx" *, *" params-str)))
          (let ((types
                 (for/list ((param-str (in-list params-strs)))
                   (match (regexp-split #rx" +" param-str)
                     [(list type name) (ffi-type<-str-type type)]))))
            (with-syntax ((func func)
                          ((type ...) types)
                          (return-type return-type))
              (syntax/loc stx
                (define-f-function func (_fun type ... -> return-type))))))))]))


(defffi "int foo(float x,int y, int z)")