#lang racket

(provide pos
         find-definition
         completions
         jump-to)

(require syntax/modread
         drracket/check-syntax
         "trace.rkt")

(define (find-definition path id)
  (define tr (new-tracer path))
  (check-syntax tr path)
  (send tr get-definition id))

(define (jump-to path from)
  (define tr (new-tracer path))
  (check-syntax tr path)
  (send tr jump-to from))

(define (completions path pos)
  (define tr (new-tracer path))
  (check-syntax tr path)
  (send tr get-completions pos))

(define (check-syntax tracer path)
  (define ns (make-base-namespace))

  (parameterize ([current-annotations tracer]
                 [current-namespace ns])
    (define-values (expanded-expression expansion-completed)
      (make-traversal (current-namespace) (current-load-relative-directory)))
    (define port (open-input-file path))
    (port-count-lines! port)
    (with-handlers ([exn? (report-error tracer)])
      (expanded-expression
       (expand
        (with-module-reading-parameterization
          (lambda ()
            (read-syntax path port))))))
    (expansion-completed)))

(define tracer (make-hash))

(define (new-tracer path)
  (if (hash-has-key? tracer path)
      (hash-ref tracer path)
      (let ([tr (make-tracer path)])
        (hash-set! tracer path tr)
        tr)))
