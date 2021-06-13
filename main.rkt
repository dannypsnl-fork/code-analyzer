#lang racket

(provide (struct-out binding)
         init-check
         get-definitions
         find-definition
         completions
         jump-to-definition
         get-references
         ; pos->lspposion
         binding->Range
         Pos->pos)

(require racket/class
         racket/list
         data/interval-map
         syntax/modread
         drracket/check-syntax
         framework
         "helper.rkt"
         "lsp-pos.rkt")

(define (init-check path)
  (define tr (new-tracer path))
  (check-syntax tr path))

(define (get-definitions path)
  (define tr (new-tracer path))
  (check-syntax tr path)
  (send tr get-definitions))

(define (find-definition path id)
  (define tr (new-tracer path))
  (check-syntax tr path)
  (send tr get-definition id))

(define (jump-to-definition path from)
  (define tr (new-tracer path))
  (check-syntax tr path)
  (send tr jump-to-def from))

(define (binding->Range path r)
  (define tr (new-tracer path))
  (match-define (binding _ start end _) r)
  (send tr pos->lsppos start end))

(define (Pos->pos path pos)
  (define tr (new-tracer path))
  (send tr lsppos->pos pos))

(define (completions path pos)
  (define tr (new-tracer path))
  (check-syntax tr path)
  (send tr get-completions pos))

(define (get-references path id)
  (define tr (new-tracer path))
  (check-syntax tr path)
  (send tr get-references id))

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

(define (make-tracer path)
  (define doc-text (new racket:text%))
  (send doc-text insert (port->string (open-input-file path)) 0)
  (new build-trace%
       [src path]
       [doc-text doc-text]))

(define build-trace%
  (class (annotations-mixin object%)
    (init-field src doc-text)

    (define errors empty)
    (define warnings empty)
    (define semantic-coloring (make-interval-map))
    (define hovers (make-interval-map))
    (define bindings (make-interval-map))
    (define definitions (make-hasheq))
    (define references (make-hash))
    (define require-locations empty)
    (define documentation empty)
    (define tails (make-hasheq))
    (define completions empty)

    (define/public (get-definitions)
      (hash-values definitions))
    (define/public (get-definition id)
      (hash-ref definitions id #f))
    (define/public (get-completions pos)
      (append completions (interval-map-ref bindings pos '())))
    (define/public (jump-to-def pos)
      (interval-map-ref bindings pos #f))
    (define/public (get-references id)
      (hash-ref references (send this get-definition id) #f))

    (define/public (pos->lsppos start end)
      (start/end->Range doc-text start end))
    (define/public (lsppos->pos pos)
      (Pos->abs-pos doc-text pos))

    ;; Getters
    (define/public (get-errors) errors)
    (define/public (get-warnings) warnings)
    (define/public (get-diagnostics) (append errors warnings))
    (define/public (get-semantic-coloring) semantic-coloring)
    (define/public (get-hovers) hovers)
    ;; Bindings to locations in current file.
    (define/public (get-bindings) bindings)
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
                      start-text start-pos-left start-pos-right start-px start-py
                      end-text end-pos-left end-pos-right end-px end-py
                      actual? level require-arrow? name-dup?)
      (define loc
        (if require-arrow?
            (let ([from-path (syntax->datum start-text)])
              (find-definition from-path (syntax->datum end-text)))
            (binding (syntax->datum start-text) start-pos-left start-pos-right src)))
      (interval-map-set! bindings end-pos-left end-pos-right
                         loc)
      (add-reference! references loc
                      (binding (syntax->datum end-text) end-pos-left end-pos-right src)))

    (define/override (syncheck:add-mouse-over-status
                      text pos-left pos-right hover-content)
      (interval-map-set! hovers pos-left pos-right hover-content))

    (define/override (syncheck:add-text-type _text pos-left pos-right text-type)
      (void))

    (define/override (syncheck:add-jump-to-definition
                      text pos-left pos-right id filename submods)
      (void))

    (define/override (syncheck:add-definition-target
                      text start end id mods)
      (hash-set! definitions id (binding (syntax->datum text) start end src)))

    (define/override (syncheck:add-require-open-menu
                      text start-pos end-pos file)
      (set! require-locations
            (cons (link start-pos end-pos
                        text
                        (string-append "file://" (path->string file)))
                  require-locations)))

    (define/override (syncheck:add-docs-menu
                      text start-pos end-pos key the-label path
                      definition-tag tag)
      (define doc-uri (format "file://~a#~a" path tag))
      (set! documentation (cons (link start-pos end-pos text doc-uri) documentation)))

    (define/override (syncheck:add-prefixed-require-reference
                      req-src req-pos-left req-pos-right)
      ;; Required to avoid arity error.
      (void))

    (define/override (syncheck:add-unused-require
                      req-src req-pos-left req-pos-right)
      (add-warning
       (warning "warn:unused-require" "Unused require."
                ;; line and column unknown
                (list
                 (srcloc src #f #f req-pos-left
                         (- req-pos-right req-pos-left))))))

    (define/override (syncheck:color-range text start end style-name)
      (define type (substring style-name 22))
      (when (not (equal? type "unused-require"))
        (interval-map-set! semantic-coloring (add1 start) (add1 end)
                           (string->symbol type))))

    (super-new)))
