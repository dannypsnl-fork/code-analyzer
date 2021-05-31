#lang racket/base

(define x 1)
(define y 1)
(define z 1)

(module+ test
  (require rackunit
           "../main.rkt")

  (check-equal? (find-definition "get-def1.rkt" 'x)
                (pos 27 28))
  (check-equal? (find-definition "get-def1.rkt" 'y)
                (pos 40 41))
  (check-equal? (find-definition "get-def1.rkt" 'z)
                (pos 53 54))
  )
