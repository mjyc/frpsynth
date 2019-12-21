#lang rosette/safe

(provide (all-defined-out))

(require
 (prefix-in r/ (only-in rosette/base/base map filter))
 rosette/lib/angelic rosette/lib/match
 (prefix-in l/ "../lang.rkt")
 "../lib.rkt")


; Data types

(define noevent '())
(define noevent? null?)
; shortcuts
(define nevt '())
(define nevt? null?)

(struct r (idx) #:transparent)  ; resigter index

; numinputs: a integer value
; instructions: a list of instructions
(struct program (numinputs instructions) #:transparent)


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


; Interpreters

(define (instruction-interpret inst)
  (match inst
    [(l/map a b) (map a (instruction-interpret b))]
    [(l/filter a b) (filter a (instruction-interpret b))]
    [_ inst]
    )
  )
