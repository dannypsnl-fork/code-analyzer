#lang racket/base

(define x 1)
(define y 1)
(define z 1)

(module+ test
  (require rackunit
           "../main.rkt")

  (check-equal? (find-definition "get-def1.rkt" 'x)
                (binding 'x 27 28 "get-def1.rkt"))
  (check-equal? (find-definition "get-def1.rkt" 'y)
                (binding 'y 40 41 "get-def1.rkt"))
  (check-equal? (find-definition "get-def1.rkt" 'z)
                (binding 'z 53 54 "get-def1.rkt"))
  )
