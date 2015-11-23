#lang racket
(require profile)
(require racket/gui/base)
(require sgl/gl)
(require ffi/unsafe
         ffi/unsafe/define)
(require (for-syntax syntax/parse))

(require (for-syntax racket/string))

(define shaders-lib (ffi-lib "/Users/arturalkaim/Shaders/lib/libShaders"))

;(define-ffi-definer define-test (ffi-lib "/Users/arturalkaim/Shaders/lib/libShaders"))


(define-for-syntax (ffi-type-from-type type)
  (case type
    ((void) '_void)
    ((int) '_int)
    ((short) '_short)
    ((bool) '_bool)
    ((double) '_double)
    ((float) '_float)
    ((uint) '_uint)
    ((string) '_string)
    (else '_pointer)))

(define-for-syntax (convert-type name param type)
  (case type
    ((void) #`(void #,param))
    ((int short uint ushort long ulong) #`(integer #,param)) ;;Requires extra checks
    ((bool) #`(boolean #,param))
    ((float double) #`(real #,param))
    ((string) #`(string #,param))
    (else #`(if (cpointer-has-tag? #,param '#,type)
                #,param
                (error '#,name
                       "ffi error: expected ~a type in argument ~a but got ~a"
                       '#,type
                       '#,param
                       #,param)))))

(define-for-syntax (ffi-sig-from-type in out)
  `(_fun ,@(map ffi-type-from-type in) -> ,(ffi-type-from-type out)))


(define-syntax (ffi stx)
  (syntax-case stx ()
    ((_ (name ffi-name) (in ...) out)
     (let ((ins (syntax->list #'(in ...))))
       (with-syntax (((param ...)
                      (map (lambda (param)
                             (if (identifier? param)
                                 (car (generate-temporaries (list param)))
                                 (car (syntax->list param))))
                           ins))
                     ((type ...)
                      (map (lambda (param)
                             (if (identifier? param)
                                 param
                                 (cadr (syntax->list param))))
                           ins)))
         (quasisyntax/loc stx
           (begin
             (provide name)
             (define name
               (let ((ffi-func 
                      (get-ffi-obj ffi-name
                                   shaders-lib
                                   #,(ffi-sig-from-type (syntax->datum #'(in ...))
                                                        (syntax->datum #'out))
                                   (make-not-available ffi-name))))
                 (lambda (param ...)
                   (let #,(map (lambda (param type)
                                 (list param
                                       (convert-type #'name param type)))
                               (syntax->list #'(param ...))
                               (syntax->datum #'(type ...)))
                     (call-in-opengl
                         (thunk
                     #,(if (eq? (ffi-type-from-type (syntax->datum #'out)) '_pointer)
                           #`(let ((result (ffi-func param ...)))
                               (cpointer-push-tag! result '#,(syntax->datum #'out))
                               result)
                           #`(ffi-func param ...)))))))))))))
    ((def name (in ...) out)
     (quasisyntax/loc stx
       (def (name #,(lowerCamelCase (symbol->string (syntax->datum #'name))))
         (in ...) out)))))

(define-for-syntax (lowerCamelCase str)
  (let ((words (regexp-split #rx"-" str)))
    (string-append* (car words) (map string-titlecase (cdr words)))))

(ffi go (int float) void)

(define-sgl go (_fun _int _float -> _void))

;;;;;;;;;;;############;;;;;;;;;;;############;;;;;;;;;;;############;;;;;;;;;;;############;;;;;;;;;;;############;;;;;;;;;;;


(define (expected type-str v)
  (raise-type-error 'wrong-type type-str v))

(define (check-expected type type-str v)
  (if (type v)
      v
      (expected type-str v)))

(define (void val)
  (check-expected void? "void" val))

(define (non-void val)
  (check-expected (lambda (v) (not (void? v))) "non void" val))

(define (string val)
  (check-expected string? "string" val))

(define (real val)
  (exact->inexact (check-expected number? "number" val)))

(define (positive-real val)
  (exact->inexact
   (check-expected
    (lambda (v) (and (number? v) (> v 0)))
    "positive number" val)))

(define (boolean val)
  (check-expected boolean? "boolean" val))

(define (boolean-true val)
  (check-expected identity "true" val))

(define (integer val)
  (check-expected integer? "integer" val))

(define (number val)
  (check-expected number? "number" val))