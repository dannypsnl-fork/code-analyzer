#lang racket

(provide pos
         find-definition
         completions)

(require "trace.rkt")

(define (find-definition path id)
  (define tr (new-tracer path))
  (send tr check-syntax)
  (send tr get-definition id))

(define (completions path pos)
  (define tr (new-tracer path))
  (send tr check-syntax)
  (send tr get-completions pos))

(define tracer (make-hash))

(define (new-tracer path)
  (if (hash-has-key? tracer path)
      (hash-ref tracer path)
      (let ([tr (make-tracer path)])
        (hash-set! tracer path tr)
        tr)))
