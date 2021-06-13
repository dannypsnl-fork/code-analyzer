#lang racket

(provide (all-defined-out))

(require (for-syntax racket/base
                     syntax/parse)
         racket/class
         racket/contract/base
         racket/match
         "json-util.rkt")

(define-match-expander Pos
  (λ (stx)
    (syntax-parse stx
      [(_ #:line l #:char c)
       (syntax/loc stx
         (hash-table ['line (? exact-nonnegative-integer? l)]
                     ['character (? exact-nonnegative-integer? c)]))]))
  (λ (stx)
    (syntax-parse stx
      [(_ #:line l #:char c)
       (syntax/loc stx
         (hasheq 'line l
                 'character c))])))

(define-json-expander Range
  [start any/c]
  [end any/c])

(define (abs-pos->Pos t pos)
  (define line (send t position-paragraph pos))
  (define line-begin (send t paragraph-start-position line))
  (define char (- pos line-begin))
  (Pos #:line line #:char char))

(define (Pos->abs-pos t pos)
  (match-define (Pos #:line line #:char char) pos)
  (line/char->pos t line char))

(define (line/char->pos t line char)
  (+ char (send t paragraph-start-position line)))

(define (start/end->Range t start end)
  (Range #:start (abs-pos->Pos t start) #:end (abs-pos->Pos t end)))
