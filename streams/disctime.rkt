#lang rosette/safe

(provide (all-defined-out))

(require
 (prefix-in r/ (only-in rosette/base/base map filter))
 (only-in rosette/base/core/safe argument-error)
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


; Interpreters

(define (instruction-interpret inst reg)
  ; (displayln (list "inst" inst))
  ; (displayln (list "reg" reg))
  (match inst
    ; TOOD: match constants
    [(l/register idx) (list-ref reg idx)]
    [(l/map a b) (map a (instruction-interpret b reg))]
    [(l/filter a b) (filter a (instruction-interpret b reg))]
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
