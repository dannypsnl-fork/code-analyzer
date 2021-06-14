#lang racket/base

(define (foo x) x)

(module+ test
  (require rackunit
           "../main.rkt")

  (check-equal? (get-documentation "get-document1.rkt")
                (list (link 20 26 'define
                            "file:///Applications/Racket v8.1/doc/reference/define.html#(form._((lib._racket/private/base..rkt)._define))"))))
