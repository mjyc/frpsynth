#lang rosette/safe

(provide (all-defined-out))

(require
 (only-in racket/base build-list for/list)
 (only-in racket/string string-join) ; don't use with sym-vars
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
    [(l/mapTo a b) (mapTo a (instruction-interpret b reg))]
    [(l/filter a b) (filter a (instruction-interpret b reg))]
    [(l/scan a b c) (scan a b (instruction-interpret c reg))]
    [(l/merge a b) (merge
      (instruction-interpret a reg) (instruction-interpret b reg))]
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


; Utils

(define (instruction->string instruction)
  (match instruction
    [(l/register idx) (format "stream~a" idx)]
    [(l/map a b)
      (format "map(~a, ~a)" a (instruction->string b))]
    [(l/mapTo a b)
      (format "mapTo(~a, ~a)" a (instruction->string b))]
    [(l/filter a b)
      (format "filter(~a, ~a)" a (instruction->string b))]
    [(l/scan a b c)
      (format "scan(~a, ~a, ~a)" a b (instruction->string c))]
    [(l/merge a b)
      (format "merge(~a, ~a)"
        (instruction->string a)
        (instruction->string b)
        )]
    [_ (format "~a" instruction)]
    )
  )

(define (program->string program)
  (define insts (l/program-instructions program))
  (string-join (for/list ([i (build-list (length insts) identity)])
    (define inst (list-ref insts i))
    (format "var stream~a = ~a;\n" i (instruction->string inst))
    ) "")
  )
