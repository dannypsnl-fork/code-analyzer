#lang racket/base

(define x 1)

x

(module+ test
  (require racket/class
           "trace.rkt")

  (define tr (check-syntax "test.rkt"))
  (send tr get-references))
