#lang racket

(module+ main
  (require racket/cmdline
           "trace.rkt")

  (command-line
    #:program "code-analyzer"
    #:args (file)
    (define tr (make-tracer file))
    (send tr check-syntax)
    (displayln (send tr get-errors))
    (displayln (send tr get-require-locations))
    ))
