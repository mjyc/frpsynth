#lang rosette/safe

(require
  (only-in racket/base procedure-arity build-list for/list in-range)
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

(define (??instruction insts [inputs '(#f)])
  (define r1 (apply choose* inputs))
  (define r2 (apply choose* inputs))
  (define r3 (apply choose* inputs))
  (apply choose*
    (for/list ([constructor insts])
      (cond
        [(= 1 (procedure-arity constructor))
          (constructor (l/register r1))]
        [(= 2 (procedure-arity constructor))
          (constructor (l/register r1) (l/register r2))]
        [else
          (constructor (l/register r1) (l/register r2) (l/register r3))]
        )
      )
    )
  )

(define (??program n k insts)
  (l/program
    n
    (for/list ([output (in-range n (+ n k))])
     (??instruction insts (build-list output identity)))))
