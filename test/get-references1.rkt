#lang racket

(define foo 1)

foo foo

(module+ test
  (require rackunit
           "../main.rkt")

  (check-equal? (get-references "get-references1.rkt" 'foo)
                (mutable-set (binding 'foo 30 33 "get-references1.rkt")
                             (binding 'foo 34 37 "get-references1.rkt"))))
