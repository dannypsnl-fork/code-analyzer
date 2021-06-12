#lang racket

(define (my-identity x)
  x)

(module+ test
  (require rackunit
           "../trace.rkt"
           "../main.rkt")

  (check-equal? (jump-to "jump-to-def1.rkt" 40)
                (mutable-set (binding 35 36 #f)))
  )
