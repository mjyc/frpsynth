#lang rosette/safe

(provide (all-defined-out))

(require
  (only-in racket/base for/list)
  (only-in racket/list make-list) ; don't use with sym-vars
  (only-in racket/string string-join) ; don't use with sym-vars
  rosette/lib/angelic rosette/lib/match
  (only-in rosette/base/core/safe argument-error)
  (prefix-in r/ (only-in rosette/base/base map filter))
  (prefix-in l/ "../lang.rkt")
  "../lib.rkt"
  )


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

(define (mapTo val stream)
  ; (displayln (list "val" val))
  ; (displayln (list "stream" stream))
  (r/map (lambda (x) (if (nevt? x) x val)) stream)
  )

(define (filter predicate stream)
  (r/map (lambda (x) (if (or (nevt? x) (not (predicate x))) nevt x)) stream)
  )

(define (scan accumulator seed stream)
  (define (fold lst acc)
    (cond
      [(empty? lst) '()]
      [else
        (define x (first lst))
        (define v
          (if (empty? x) acc (accumulator x acc)))
        (cons v (fold (rest lst) v))
        ]
      )
    )
  (fold stream seed)
  )

; if two events at same time are 'noevent', it takes the first event
(define (merge stream1 stream2)
  (r/map
    (lambda (a b) (if (noevent? b) a b))
    stream1 stream2)
  )

; TODO: define the length of 1 period
(define (delay period stream)
  (append
    (make-list period noevent)
    (reverse (drop (reverse stream) period)))
  )

(define (delay-lifted period stream)
  (define out-streams
    (list
      (delay 1 stream)
      (delay 2 stream)
      (delay 3 stream)
      (delay 4 stream)
      (delay 5 stream)
      ))
  (list-ref out-streams (- period 1))
  )


; Sources

; TODO: define the length of 1 period
(define (from-diagram diagramString)
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

(define (instruction-interpret inst regs)
  ; (displayln (list "inst" inst))
  ; (displayln (list "regs" regs))
  (match inst
    ; TODO: match constants
    [(l/register idx) (list-ref regs idx)]
    [(l/map a b) (map a (instruction-interpret b regs))]
    [(l/mapTo a b) (mapTo a (instruction-interpret b regs))]
    [(l/filter a b) (filter a (instruction-interpret b regs))]
    [(l/delay a b) (delay-lifted a (instruction-interpret b regs))]
    [(l/scan a b c) (scan a b (instruction-interpret c regs))]
    [(l/merge a b) (merge
      (instruction-interpret a regs) (instruction-interpret b regs))]
    [_ inst]
    )
  )

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


; Holes

(define (??stream create-event length)
  (for/list ([i length])
    (define-symbolic* sb boolean?)
    (if sb
      (create-event)
      noevent
      )
    )
  )
