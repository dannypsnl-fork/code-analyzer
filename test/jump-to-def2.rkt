#lang racket

(require "jump-to-def2-from.rkt")

(my-identity 200)

(module+ test
  (require rackunit
           "../main.rkt")

  (check-equal? (jump-to-definition "jump-to-def2.rkt" 54)
                (binding 46 57 "jump-to-def2-from.rkt"))
  )
