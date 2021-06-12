#lang racket

(define (my-identity x)
  x)

(module+ test
  (require rackunit
           "../main.rkt")

  (check-equal? (jump-to-definition "jump-to-def1.rkt" 40)
                (binding 35 36 "jump-to-def1.rkt"))
  )
