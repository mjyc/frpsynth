#lang rosette/safe

(provide (all-defined-out))

(require (prefix-in r/ (only-in rosette/base/base map filter))
 rosette/lib/angelic rosette/lib/match
 (prefix-in l/ "../lang.rkt")
 "../lib.rkt")


; Data types

(define noevent '())
(define noevent? null?)
; shortcuts
(define nevt '())
(define nevt? null?)


; Operators

(define (map fn stream)
  (r/map (lambda (x) (if (nevt? x) x (fn x))) stream)
  )

(define (filter predicate stream)
  (r/map (lambda (x) (if (or (nevt? x) (not (predicate x))) nevt x)) stream)
  )


; Sources

(define (from-diagram diagramString)
  (define interval 20)
  (define characters (cdr (drop-right (string-split diagramString "") 1)))
  (define (rec chars outstream)
    (match chars
      [(cons "-" xs) (rec xs (cons nevt outstream))]
      [(cons x xs) (rec xs (cons x outstream))]
      ['() outstream]
      )
    )
  (reverse (rec characters '()))
  )


; Syntax

(define (interpret p)
  (match p
    [(l/map a b) (map a (interpret b))]
    [(l/filter a b) (filter a (interpret b))]
    [_ p]
    )
  )
