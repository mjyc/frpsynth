#lang rosette/safe

(require rackunit rackunit/text-ui
  (only-in racket/base exn:fail?)
  (prefix-in s/ "../../streams/disctime.rkt")
  (prefix-in l/ "../../lang.rkt")
  "../../hole.rkt")

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

(define (test-delay)
  (test-case
    "test-delay"
    (check-equal?
      (s/delay 2 (list 1 2 3))
      (list s/noevent s/noevent 1)
      )
    )
  )

(define (test-delay-lifted)
  (test-case
    "test-delay-lifted"
    (check-exn
      exn:fail?
      (lambda () (s/delay-lifted 0 (list 1 2 3)))
      )
    (check-equal?
      (s/delay-lifted 1 (list 1 2 3))
      (list s/nevt 1 2)
      )
    (check-equal?
      (s/delay-lifted 2 (list 1 2 3))
      (list s/nevt s/nevt 1)
      )
    ; creates empty stream
    (check-equal?
      (s/delay-lifted 3 (list 1 2 3))
      (list s/nevt s/nevt s/nevt)
      )
    )
  )


; Test sources

(define (test-from-diagram)
  (test-case
    "test-from-diagram"
    (define outstream1 (s/from-diagram "1234"))
    (define expected1
      (list 1 2 3 4))
    (check-equal? outstream1 expected1)
    (define outstream2 (s/from-diagram "5--6"))
    (define expected2 (list 5 s/noevent s/noevent 6))
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
    (define inputs (list (list 0 1)))
    (define output (s/program-interpret prog inputs))
    (check-equal? output (list 1 s/noevent))

    (define numinputs2 2)
    (define prog2
      (l/program
        numinputs2
        (list (l/mapTo 1 (l/register 0))
          (l/mapTo -1 (l/register 1))
          (l/merge (l/register 2) (l/register 3))
          (l/scan + 0 (l/register 4))
          )))
    (define inputs2
      (list
        (list #t s/noevent #t s/noevent)
        (list s/noevent #f s/noevent #f)
        ))
    (check-equal?
      (s/program-interpret prog2 inputs2)
      (list 1 0 1 0))
    )
  )


; Test solver-aided programming

(current-bitwidth #f)
(define-symbolic x y integer?)
(define-symbolic b boolean?)

(define (test-solve)
  (test-case
    "test-solve"
    (define inst (l/map add1 (list x y)))
    (define sol
      (time
        (solve
          (assert (equal?
            (s/instruction-interpret inst '())
            (list 1 1))))))
    (check-true (sat? sol))
    (check-equal? (evaluate (list x y) sol) (list 0 0))

    (define inst2
      (if b
        (l/map add1 (list x y))
        (l/filter odd? (list x y))
        )
      )
    (define sol2
      (time
        (solve
          (assert (equal?
            (s/instruction-interpret inst2 '())
            (list 2 2))))))
    (check-true (sat? sol2))
    (check-equal? (evaluate (list b x y) sol2) (list #t 1 1))
    )
  )

(define (test-angexe)
  (test-case
    "test-angexe"
    (define numinputs 2)
    (define spec
      (l/program
        numinputs
        (list
          (l/mapTo 1 (l/register 0))
          (l/mapTo -1 (l/register 1))
          (l/merge (l/register 2) (l/register 3))
          (l/scan + 0 (l/register 4))
          )))
    ; (displayln (list "spec" spec))
    (define sketch
      (??program numinputs (length (l/program-instructions spec))
        (list
          l/merge
          (lambda (x) (l/map add1 x))
          (lambda (x) (l/mapTo (??constant) x))
          (lambda (x) (l/filter odd? x))
          (lambda (x) (l/scan + 0 x))
          )
        )
      )
    ; (displayln (list "sketch" (l/program->string sketch)))
    (define inputs
      (list
        (list #t s/noevent #t s/noevent)
        (list s/noevent #f s/noevent #f)
        ))
    ; (displayln (list "inputs" inputs))
    ; (displayln "(s/program-interpret spec inputs):")
    ; (displayln (s/program-interpret spec inputs))

    (define M
      (time
        (solve
          (assert
            (equal?
              (s/program-interpret spec inputs)
              (s/program-interpret sketch inputs)
              ))))
      )
    (check-true (sat? M))
    (define result (evaluate sketch M))
    ; (displayln "angexe result:")
    ; (displayln (l/program->string result))
    (check-equal? (s/program-interpret result inputs) (list 1 0 1 0))
    )
  )

(define (test-synth)
  (test-case
    "test-synth"
    (define numinputs 2)
    (define spec
      (l/program
        numinputs
        (list (l/mapTo 1 (l/register 0))
          (l/mapTo -1 (l/register 1))
          (l/merge (l/register 2) (l/register 3))
          (l/scan + 0 (l/register 4))
          )))
    ; (displayln (list "spec" spec))
    (define sketch
      (??program numinputs (length (l/program-instructions spec))
        (list
          l/merge
          (lambda (x) (l/map add1 x))
          (lambda (x) (l/mapTo (??constant) x))
          (lambda (x) (l/filter odd? x))
          (lambda (x) (l/scan + 0 x))
          )
        )
      )
    ; (displayln (list "sketch" sketch))
    (define len 4)
    (define sym-inputs
      (list
        (s/??stream (lambda () #t) len)
        (s/??stream (lambda () #f) len)))

    (define M
      (time
        (synthesize
          #:forall (symbolics sym-inputs)
          #:guarantee (assert (equal?
            (s/program-interpret spec sym-inputs)
            (s/program-interpret sketch sym-inputs)
            ))))
      )
    (check-true (sat? M))
    (define result (evaluate sketch M))
    ; (displayln "synthe result:")
    ; (displayln (l/program->string result))
    (define test-inputs
      (list
        (list #t s/noevent #t s/noevent)
        (list s/noevent #f s/noevent #f)
        ))
    (check-equal?
      (s/program-interpret result test-inputs)
      (list 1 0 1 0))
    )
  )


; Main

(module+ test
  (define/provide-test-suite disctime-tests
    (test-noevent)
    (test-map)
    (test-filter)
    (test-delay)
    (test-delay-lifted)
    (test-from-diagram)
    (test-instruction-interpret)
    (test-program-interpret)
    (test-solve)
    (test-angexe)
    (test-synth)
    )
  (run-tests disctime-tests)
  )
