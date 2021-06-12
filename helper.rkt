#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/set
         racket/match
         data/interval-map)

(struct exception (code msg srclocs) #:transparent)
(struct warning (code msg srclocs) #:transparent)
(struct binding (start end require-from) #:transparent)
(struct reference (filename id) #:transparent)
(struct link (start end text file) #:transparent)

(define (exn->exception e)
  (define-values (struct-type _) (struct-info e))
  (match-define-values (code _ _ _ _ _ _ _) (struct-type-info struct-type))
  (define msg (exn-message e))
  (define srclocs (if (exn:srclocs? e) ((exn:srclocs-accessor e) e) '()))
  (exception (symbol->string code) msg srclocs))

(define ((report-error trace) exn)
  (send trace add-error (exn->exception exn)))

(define (add-completion-word! map start end val)
  (define c (mutable-set val))
  (define s (interval-map-ref map start #f))
  (when s
    (set-union! c s))
  (define e (interval-map-ref map end #f))
  (when e
    (set-union! c e))
  (interval-map-set! map start end
                     c))

(module+ test
  (require rackunit)

  (test-case "contribute completion"
             (define m (make-interval-map))
             (add-completion-word! m 0 10 'a)
             (add-completion-word! m 0 12 'b)
             (add-completion-word! m 0 6 'c)
             (check-equal? (interval-map-ref m 7)
                           (mutable-set 'a 'b))
             (check-equal? (interval-map-ref m 0)
                           (mutable-set 'a 'b 'c))
             (check-equal? (interval-map-ref m 11)
                           (mutable-set 'a 'b))))
