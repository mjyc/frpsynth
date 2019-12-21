#lang rosette/safe

(require rackunit rackunit/text-ui
 (prefix-in s/ "../../streams/disctime.rkt")
 (prefix-in l/ "../../lang.rkt"))

(provide (all-defined-out))


; Test data types

(define (test-noevent)
  (test-case
    "test-noevent"
    (check-equal? s/noevent '())
    (check-true (s/noevent? '()))
    (check-equal? s/nevt '())
    (check-true (s/nevt? '()))
    )
  )


; Test transformers

(define (test-map)
  (test-case
    "test-map"
    (check-equal?
      (s/map add1 (list 0 0 0))
      (list 1 1 1)
      )
    (check-equal?
      (s/map add1 (list 0 s/noevent 0))
      (list 1 s/noevent 1)
      )
    )
  )

(define (test-filter)
  (test-case
    "test-filter"
    (check-equal?
      (s/filter even? (list 0 1 2))
      (list 0 s/noevent 2)
      )
    )
  )


; Test sources

(define (test-from-diagram)
  (test-case
    "test-from-diagram"
    (define outstream1 (s/from-diagram "1234"))
    (define expected1
      (list "1" "2" "3" "4"))
    (check-equal? outstream1 expected1)
    (define outstream2 (s/from-diagram "5--6"))
    (define expected2 (list "5" s/noevent s/noevent "6"))
    (check-equal? outstream2 expected2)
    )
  )


; Test interpreters

(define (test-instruction-interpret)
  (test-case
    "test-instruction-interpret"
    (define inst
      (l/filter odd?
        (l/map add1 (list 0 1 2))))
    (check-equal?
      (s/instruction-interpret inst '())
      (list 1 s/nevt 3))

    (define inst2
      (l/filter odd?
        (l/map add1 (l/register 0))))
    (define input
      (list (list 0 1 2)))
    (check-equal?
      (s/instruction-interpret inst2 input)
      (list 1 s/nevt 3))
    )
  )

(define (test-program-interpret)
  (test-case
    "test-program-interpret"
    (define numinputs 1)
    (define prog
      (l/program
        numinputs
        (list
          (l/map add1 (l/register 0))
          (l/filter odd? (l/register 1))
          )))
    (define input (list (list 0 1)))
    (define output (s/program-interpret prog input))
    (check-equal? output (list 1 s/noevent))
    )
  )


; Test solver-aided programming

(current-bitwidth #f)
(define-symbolic x y integer?)

(define (test-solve)
  (test-case
    "test-solve"
    (define inst (l/map add1 (list x y)))
    (define sol (solve
      (assert (equal? (s/instruction-interpret inst '()) (list 1 1)))))
    (check-true (sat? sol))
    (check-equal? (evaluate (list x y) sol) (list 0 0))
    )
  )

(define-symbolic b boolean?)

(define (test-angexe)
  (test-case
    "test-angexe"
    (define inst
      (if b
        (l/map add1 (list x y))
        (l/filter odd? (list x y))
        )
      )
    (define sol (solve
      (assert (equal? (s/instruction-interpret inst '()) (list 2 2)))))
    (check-true (sat? sol))
    (check-equal? (evaluate (list b x y) sol) (list #t 1 1))
    )
  )


; Main

(module+ test
  (define/provide-test-suite disctime-tests
    (test-noevent)
    (test-map)
    (test-filter)
    (test-from-diagram)
    (test-instruction-interpret)
    (test-program-interpret)
    (test-solve)
    (test-angexe)
    )
  (run-tests disctime-tests)
  )
