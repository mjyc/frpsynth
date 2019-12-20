#lang rosette/safe

(provide (all-defined-out))

(require (prefix-in r/ (only-in rosette/base/base map filter))
 rosette/lib/angelic rosette/lib/match
 (prefix-in l/ "../lang.rkt")
 "../lib.rkt")


; Data types

(struct event (stamp value) #:transparent)


; Operators

(define (map fn stream)
  (define (wrapped evt)
    (event (event-stamp evt) (fn (event-value evt)))
    )
  (r/map wrapped stream)
  )

(define (filter predicate stream)
  (define (wrapped evt)
    (predicate (event-value evt))
    )
  (r/filter wrapped stream)
  )

; implements 'ap'; see https://github.com/rpominov/basic-streams#ap
(define (ap-rec streamf streamv stamp lastf lastv streamr)
  (define sr (cons (event stamp (lastf lastv)) streamr))
  (match (list streamf streamv)
    [(list '() '()) sr]
    [(list (cons (event tsf valf) sf) '())
     (ap-rec sf '() tsf valf lastv sr)]
    [(list '() (cons (event tsv valv) sv))
     (ap-rec '() sv tsv lastf valv sr)]
    [(list (cons (event tsf valf) sf) (cons (event tsv valv) sv))
     ; when tied assumes first stream has a lower stamp
     (ap-rec
      (if (<= tsf tsv) sf streamf)
      (if (<= tsf tsv) streamv sv)
      (if (<= tsf tsv) tsf tsv)
      (if (<= tsf tsv) valf lastf)
      (if (<= tsf tsv) lastv valv)
      sr)
     ]
    )
  )

(define (ap streamf streamv)
  (match (list streamf streamv)
    [(list '() '()) '()]
    [(list '() (list a ...)) '()]
    [(list (list a ...) '()) '()]
    [(list (cons (event tsf valf) sf) (cons (event tsv valv) sv))
     ; when tied, assumes first stream has a lower stamp
     (define ts (if (> tsf tsv) tsf tsv))
     (reverse (ap-rec sf sv ts valf valv '()))
     ]
    )
  )


; Sources

(define (from-diagram diagramString)
  (define interval 20)
  (define characters (cdr (drop-right (string-split diagramString "") 1)))
  (define (rec chars i outstream)
    (match chars
      [(cons "-" xs) (rec xs (add1 i) outstream)]
      [(cons x xs) (rec xs (add1 i) (cons (event (* i interval) x) outstream))]
      ['() outstream]
      )
    )
  (reverse (rec characters 0 '()))
  )


; Syntax

(define (interpret p)
  (match p
    [(l/map a b) (map a (interpret b))]
    [(l/filter a b) (filter a (interpret b))]
    [_ p]
    )
  )
