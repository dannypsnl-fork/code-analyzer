#lang racket

(define foo 1)

foo

(define (id x)
  x)

(module+ test
  (require rackunit
           "../main.rkt")

  (check-equal? (completions "completion1.rkt" 48)
                '()))
