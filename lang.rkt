#lang rosette/safe

(provide (all-defined-out))

(require
  (only-in racket/base build-list for/list)
  (only-in racket/string string-join) ; don't use with sym-vars
  rosette/lib/match
  (only-in rosette/base/core/safe argument-error)
  )


; Data types

; FRP DSL
(struct map (a1 a2) #:transparent)
(struct mapTo (a1 a2) #:transparent)
(struct filter (a1 a2) #:transparent)
(struct scan (a1 a2 a3) #:transparent)
(struct merge (a1 a2) #:transparent)

; Program DSL
(struct register (idx) #:transparent)
(struct program (numinputs instructions) #:transparent)


; Utils

(define (instruction->string instruction [mode "rkt"])
  (match mode
    ["rkt" (match instruction
      [(register idx) (format "r~a" idx)]
      [(map a b)
        (format "(map ~a ~a)" a (instruction->string b))]
      [(mapTo a b)
        (format "(mapTo ~a ~a)" a (instruction->string b))]
      [(filter a b)
        (format "(filter ~a ~a)" a (instruction->string b))]
      [(scan a b c)
        (format "(scan ~a ~a ~a)" a b (instruction->string c))]
      [(merge a b)
        (format "(merge ~a ~a)"
          (instruction->string a)
          (instruction->string b)
          )]
      [_ (format "~a" instruction)]
      )]
    ["js" (match instruction
      [(register idx) (format "stream~a" idx)]
      [(map a b)
        (format "map(~a, ~a)" a (instruction->string b))]
      [(mapTo a b)
        (format "mapTo(~a, ~a)" a (instruction->string b))]
      [(filter a b)
        (format "filter(~a, ~a)" a (instruction->string b))]
      [(scan a b c)
        (format "scan(~a, ~a, ~a)" a b (instruction->string c))]
      [(merge a b)
        (format "merge(~a, ~a)"
          (instruction->string a)
          (instruction->string b)
          )]
      [_ (format "~a" instruction)]
      )]
    [_ (argument-error 'program->string "unknown mode ~a" mode)]
    )
  )

(define (program->string program [mode "rkt"])
  (define insts (program-instructions program))
  (string-join (for/list ([i (build-list (length insts) identity)])
    (define inst (list-ref insts i))
    (match mode
      ["rkt" (format "(define r~a ~a)\n" (+ i (program-numinputs program)) (instruction->string inst mode))]
      ["js" (format "var stream~a = ~a;\n" i (instruction->string inst mode))]
      [_ (argument-error 'program->string "unknown mode ~a" mode)]
      )
    ) "")
  )
