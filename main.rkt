#lang racket

(module+ main
  (require racket/cmdline
           "trace.rkt")

  (command-line
    #:program "code-analyzer"
    #:args (file)
    (define tr (check-syntax file))
    (displayln (send tr get-errors))
    (displayln (send tr get-require-locations))
    ))
