#lang racket

(module+ test
  (require rackunit
           "../main.rkt")

  (check-equal? (completions "completion1.rkt" 13)
                '()))
