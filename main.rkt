#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/block
         syntax/parse/define)

(define-syntax (decorate stx)
  (raise-syntax-error
   #false "must be used immediately within a decorating block" stx))

(define-syntax (#%decoratable-form stx)
  (raise-syntax-error
   #false "must be used immediately within a decorating block" stx))

(define-simple-macro (decorating-block body:expr ...)
  (block (decorating-begin-loop [] body ...)))

(define-simple-macro (decorating-begin body:expr ...)
  (decorating-begin-loop [] body ...))

(define-syntax decorating-begin-loop
  (syntax-parser
    #:track-literals
    [(_ [pending-decorator ...]) #'(begin)]
    [(_ [pending-decorator ...] initial-form leftover-form ...)
     (define expanded-initial-form
       (local-expand
        #'initial-form
        (syntax-local-context)
        (list #'decorate #'define-values #'define-syntaxes #'#%decoratable-form)))
     (syntax-protect
      (syntax-parse (syntax-disarm expanded-initial-form #false)
        #:literal-sets (kernel-literals)
        #:literals (decorate #%decoratable-form)
        #:track-literals
        [(decorate ~! decorator:id)
         #'(decorating-begin-loop [pending-decorator ... decorator] leftover-form ...)]
        [(begin ~! subform:expr ...)
         #'(decorating-begin-loop [pending-decorator ...] subform ... leftover-form ...)]
        [(define-values ~! . _)
         #`(begin
             (print-decorators [pending-decorator ...] #,expanded-initial-form)
             (decorating-begin-loop [] leftover-form ...))]
        [(define-syntaxes ~! . _)
         #`(begin
             (print-decorators [pending-decorator ...] #,expanded-initial-form)
             (decorating-begin-loop [] leftover-form ...))]
        [(#%decoratable-form ~! accumulator:id form)
         #'(begin
             (accumulator [pending-decorator ...] form)
             (decorating-begin-loop [] leftover-form ...))]
        [e:expr
         #'(begin
             (print-decorators [pending-decorator ...] e)
             (decorating-begin-loop [] leftover-form ...))]))]))

(define-simple-macro (print-decorators [decorator:id ...] form)
  (begin
    (printf "~a: ~a\n" 'form (list 'decorator ...))
    form))

(module+ test
  
  (decorating-block
   (define a 1))

  (decorating-block
   (decorate foo)
   (define a 1))

  (decorating-block
   (decorate foo)
   (decorate bar)
   (decorate baz)
   (define a 1))

  (decorating-block
   (decorate foo)
   (define-syntax a #false))

  (decorating-block
   (define a 1)
   (define b 2)
   (define c 3)
   (decorate foo)
   (decorate bar)
   (decorate baz)
   (+ a b c))

  (decorating-block
   (define-simple-macro (mystruct name:id (field:id ...))
     (#%decoratable-form print-decorators (struct name (field ...) #:transparent)))
   (decorate foo)
   (decorate bar)
   (decorate baz)
   (mystruct point (x y))))
