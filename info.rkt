#lang info

(define collection "decorator")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "decorator")))

(define deps
  (list "base"))

(define build-deps
  (list "racket-doc"
        "rackunit-lib"
        "scribble-lib"))
