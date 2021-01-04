#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/block
         syntax/parse/define)

;; This is how a user signals that a form should decorate the following form.
(define-syntax (decorate stx)
  (raise-syntax-error
   #false "must be used immediately within a decorating block" stx))

;; This is how a writer of a macro such as define or struct specifies how decorators should be
;; accumulated and processed.
(define-syntax (#%decoratable-form stx)
  (raise-syntax-error
   #false "must be used immediately within a decorating block" stx))

;; Like (block body ...), except it supports decorators.
(define-simple-macro (decorating-block body:expr ...)
  (block (decorating-begin-loop [] body ...)))

;; Like (begin body ...), except it supports decorators.
(define-simple-macro (decorating-begin body:expr ...)
  (decorating-begin-loop [] body ...))

;; The core implementation of the decorator protocol.
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

        [(decorate ~! decorator:expr)
         #'(decorating-begin-loop [pending-decorator ... decorator] leftover-form ...)]
        
        [(#%decoratable-form ~! accumulator:id form)
         #'(begin
             (accumulator [pending-decorator ...] form)
             (decorating-begin-loop [] leftover-form ...))]

        [(begin ~! subform:expr ...)
         #'(decorating-begin-loop [pending-decorator ...] subform ... leftover-form ...)]

        ;; The following cases are for forms built-in to Racket that we would like to support
        ;; decorating.

        ;; If (define-values ...) was implemented as a library instead of being built-in, it could
        ;; expand to a usage of #%decoratable-form with decorate-value-definition in order to
        ;; implement the decoration semantics for definitions that we're special-casing here.
        [(define-values ~! . _)
         #`(begin
             (decorate-value-definition [pending-decorator ...] #,expanded-initial-form)
             (decorating-begin-loop [] leftover-form ...))]
        
        ;; For now we just log decorators for anything else (like syntax definitions, expressions,
        ;; submodules, or require/provide statements).
        [e:expr
         #'(begin
             (print-decorators [pending-decorator ...] e)
             (decorating-begin-loop [] leftover-form ...))]))]))

(define-syntax decorate-expression
  (syntax-parser
    #:track-literals
    [(decorate-expression [] expression:expr) #'expression]
    [(decorate-expression [initial-decorator:expr leftover-decorator:expr ...] expression:expr)
     #'(initial-decorator (decorate-expression [leftover-decorator ...] expression))]))

(begin-for-syntax
  (define-syntax-class value-definition
    #:literals (define-values)
    #:attributes ([id 1] expression)
    (pattern (define-values (id:id ...) expression:expr))))

;; Definitions are decorated by decorating their bodies. Decorators are given the defined identifiers,
;; so they can use them while generating a replacement body.
(define-simple-macro (decorate-value-definition decorators definition:value-definition)
  (define-values (definition.id ...)
    (decorate-value-definition-expression decorators (definition.id ...) definition.expression)))

(define-syntax decorate-value-definition-expression
  (syntax-parser
    #:track-literals
    [(decorate-value-definition-expression [] _ expression:expr) #'expression]
    [(decorate-value-definition-expression
      [initial-decorator:expr leftover-decorator:expr ...]
      identifiers
      expression:expr)
     #'(initial-decorator
        identifiers
        (decorate-value-definition-expression [leftover-decorator ...] identifiers expression))]))

(define-simple-macro (decorate-syntax-definition _ definition) definition)

(define-simple-macro (print-decorators [decorator:id ...] form)
  (begin
    (printf "~a: ~a\n" 'form (list 'decorator ...))
    form))

;; This is a (non-syntax) definition decorator. Definition decorators are macros that receive the list
;; of identifiers to define, and the body expression that produces their values. They expand to a
;; replacement body expression. This definition decorator prints the defined values when the
;; definition is evaluated.
(define-simple-macro (@print (id ...) expression)
  #:with (result ...) (generate-temporaries #'(id ...))
  (let-values ([(result ...) expression])
    (printf "~a = ~a\n" 'id result) ...
    (values result ...)))

(module+ test

  (decorating-block

   ;; Prints a = 1
   (decorate @print)
   (define a 1)

   ;; Prints b = 2
   (decorate @print)
   (define b 2)

   ;; Prints c = 3
   (decorate @print)
   (define c 3)

   ;; Prints:
   ;; d = 4
   ;; e = 5
   ;; f = 6
   (decorate @print)
   (define-values (d e f) (values 4 5 6))))
