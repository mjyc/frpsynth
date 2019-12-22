#lang rosette/safe

(require
  rosette/lib/angelic
  (prefix-in l/ "./lang.rkt")
  )

(provide (all-defined-out))


; Symbolic Syntax

(define (??constant)
  (define-symbolic* ci integer?)
  (define-symbolic* cb boolean?)
  (choose* ci cb))

(define (??register)
  (define-symbolic* si integer?)
  (l/register si))

(define (??binfactory)
  (choose*
    (l/merge (??register) (??register)))
  )

(define (??unoperator)
  (choose*
    (l/map add1 (??register))
    (l/mapTo (??constant) (??register))
    (l/filter odd? (??register))
    )
  )

(define (??binoperator)
  (choose*
    (l/scan + 0 (??register))
    )
  )

(define (??instruction)
  (choose*
    (??binfactory)
    (??unoperator)
    ; (??binoperator)
    )
  )
