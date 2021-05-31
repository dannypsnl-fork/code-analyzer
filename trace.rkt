#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/list
         racket/match
         data/interval-map
         drracket/check-syntax
         syntax/modread)

(struct exception (code msg srclocs) #:transparent)
(struct warning (code msg srclocs) #:transparent)
(struct binding (start end require?) #:transparent)
(struct reference (filename id) #:transparent)
(struct link (start end text file) #:transparent)

(define (exn->exception e)
  (match-define-values (struct-type _) (struct-info e))
  (match-define-values (code _ _ _ _ _ _ _) (struct-type-info struct-type))
  (define msg (exn-message e))
  (define srclocs (if (exn:srclocs? e) ((exn:srclocs-accessor e) e) '()))
  (exception (symbol->string code) msg srclocs))

(define build-trace%
  (class (annotations-mixin object%)
    (init-field src)

    (define errors empty)
    (define warnings empty)
    (define semantic-coloring (make-interval-map))
    (define hovers (make-interval-map))
    (define bindings (make-interval-map))
    (define definitions (make-hasheq))
    (define references (make-interval-map))
    (define require-locations empty)
    (define documentation empty)
    (define tails (make-hasheq))

    ;; Getters
    (define/public (get-errors) errors)
    (define/public (get-warnings) warnings)
    (define/public (get-diagnostics) (append errors warnings))
    (define/public (get-semantic-coloring) semantic-coloring)
    (define/public (get-hovers) hovers)
    ;; Bindings to locations in current file.
    (define/public (get-bindings) bindings)
    ;; Definitions are things that can be referenced.
    (define/public (get-definitions) definitions)
    ;; References locations in other files.
    (define/public (get-references) references)
    ;; References a file.
    (define/public (get-require-locations) require-locations)
    (define/public (get-documentation) documentation)
    ;; Tail recursion
    (define/public (get-tails) tails)

    (define/public (add-error err)
      (set! errors (cons err errors))
      void)

    (define/public (add-warning warn)
      (set! warnings (cons warn warnings))
      void)

    (define/override (syncheck:find-source-object stx)
      ;; skip annotations if source-object's source location is
      ;; from a different file.
      (and (equal? src (syntax-source stx))
           stx))

    (define/override (syncheck:add-tail-arrow from-text from-pos to-text to-pos)
      (hash-set! tails from-pos to-pos)
      void)

    (define/override (syncheck:add-arrow/name-dup/pxpy
                      _start-text start-pos-left start-pos-right start-px start-py
                      _end-text end-pos-left end-pos-right end-px end-py
                      actual? level require-arrow? name-dup?)
      (interval-map-set! bindings end-pos-left end-pos-right
                         (binding start-pos-left start-pos-right require-arrow?))
      void)

    (define/override (syncheck:add-mouse-over-status
                      _text pos-left pos-right hover-content)
      (interval-map-set! hovers pos-left pos-right hover-content)
      void)

    (define/override (syncheck:add-text-type _text pos-left pos-right text-type)
      void)

    (define/override (syncheck:add-jump-to-definition
                      _text pos-left pos-right id filename submods)
      (interval-map-set! references pos-left pos-right
                         (reference filename id))
      void)

    (define/override (syncheck:add-definition-target
                      _text pos-left pos-right id mods)
      (hash-set! definitions id '(pos-left . pos-right))
      void)

    (define/override (syncheck:add-require-open-menu
                      text start-pos end-pos file)
      (set! require-locations
            (cons (link start-pos end-pos
                        text
                        (string-append "file://" (path->string file)))
                  require-locations))
      void)

    (define/override (syncheck:add-docs-menu
                      text start-pos end-pos key the-label path
                      definition-tag tag)
      (define doc-uri (format "file://~a#~a" path tag))
      (set! documentation (cons (link start-pos end-pos text doc-uri) documentation))
      void)

    (define/override (syncheck:add-prefixed-require-reference
                      _req-src req-pos-left req-pos-right)
      ;; Required to avoid arity error.
      void)

    (define/override (syncheck:add-unused-require
                      _req-src req-pos-left req-pos-right)
      (add-warning
       (warning "warn:unused-require" "Unused require."
                ;; line and column unknown
                (list
                 (srcloc src #f #f req-pos-left
                         (- req-pos-right req-pos-left)))))
      void)

    (define/override (syncheck:color-range _text start end style-name)
      (define type (substring style-name 22))
      (when (not (equal? type "unused-require"))
        (interval-map-set! semantic-coloring (add1 start) (add1 end)
                           (string->symbol type)))
      void)

    (super-new)))

(define ((report-error trace) exn)
  (send trace add-error (exn->exception exn)))

(define (check-syntax path)
  (define ns (make-base-namespace))
  (define trace (new build-trace% [src path]))

  (parameterize ([current-annotations trace])
    (define-values (expanded-expression expansion-completed)
      (make-traversal ns path))
    (define port (open-input-file path))
    (port-count-lines! port)
    (parameterize ([current-namespace ns])
      (with-handlers ([exn? (report-error trace)])
        (expanded-expression
         (expand
          (with-module-reading-parameterization
            (lambda ()
              (read-syntax path port)))))))
    (expansion-completed))
  trace)
