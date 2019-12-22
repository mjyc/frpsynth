#lang rosette/safe

(provide (all-defined-out))

(require
 (only-in racket/string string-join) ; don't use with sym-vars
 (prefix-in r/ (only-in rosette/base/base map filter))
 (only-in rosette/base/core/safe argument-error)
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

(define (mapTo val stream)
  (define (wrapped evt)
    (event (event-stamp evt) val)
    )
  (r/map wrapped stream)
  )

(define (filter predicate stream)
  (define (wrapped evt)
    (predicate (event-value evt))
    )
  (r/filter wrapped stream)
  )

(define (scan accumulator seed stream)
  '()
  )

(define (merge stream1 stream2)
  (displayln "(sort (append stream1 stream2))")
  (displayln (sort (append stream1 stream2) (lambda (a b)
    (< (event-stamp a) (event-stamp b))
    )))
  ; (define (wrapped evt)
  ;   (event (event-stamp evt) val)
  ;   )
  ; (r/map wrapped stream)
  (sort (append stream1 stream2) (lambda (a b)
    (< (event-stamp a) (event-stamp b))
    ))
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


; Interpreters

(define (instruction-interpret inst regs)
  ; (displayln (list "inst" inst))
  ; (displayln (list "regs" regs))
  (match inst
    ; TODO: match constants
    [(l/register idx) (list-ref regs idx)]
    [(l/map a b) (map a (instruction-interpret b regs))]
    [(l/mapTo a b) (mapTo a (instruction-interpret b regs))]
    [(l/filter a b) (filter a (instruction-interpret b regs))]
    [(l/scan a b c) (scan a b (instruction-interpret c regs))]
    [(l/merge a b) (merge
      (instruction-interpret a regs) (instruction-interpret b regs))]
    [_ inst]
    )
  )

; TODO: take it out?
(define (program-interpret prog inputs)
  ; (displayln (list "prog" prog))
  ; (displayln (list "inputs" inputs))
  (unless (= (l/program-numinputs prog) (length inputs))
    (argument-error 'program-interpret "expected ~a inputs, given ~a" (l/program-numinputs prog) inputs))
  (define (exec insts regs)
    (cond
      [(null? insts) regs]
      [else
        (exec
          (rest insts)
          (append regs (list (instruction-interpret (first insts) regs)))
          )
        ]
      ))
  (define regs
    (exec (l/program-instructions prog) inputs))
  ; (displayln (list "regs" regs))
  (define output
    (if (or (empty? regs) (= (length regs) (length inputs)))
      '()
      (first (reverse regs)))
    )
  ; (displayln (list "output" output))
  output
  )
