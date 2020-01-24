#lang rosette/safe

(require
  (only-in racket/base build-list)
  rosette/lib/angelic
  (prefix-in l/ "./lang.rkt")
  )

(provide (all-defined-out))


; Symbolic Syntax

(define (??integer)
  (define-symbolic* ci integer?)
  ci
  )

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
    (??binoperator)
    )
  )

(define (??program numinputs maxnumregs)
  (define ret (??integer))
  (l/program
    numinputs
    (build-list maxnumregs
      (lambda (x) (??instruction)))
    )
  )
