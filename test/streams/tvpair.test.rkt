#lang rosette/safe

(require rackunit rackunit/text-ui
 (prefix-in s/ "../../streams/tvpair.rkt")
 (prefix-in l/ "../../lang.rkt")
 "../../hole.rkt"
 (prefix-in logger- "../../logger.rkt")
 )

(provide (all-defined-out))


; Test data types

(define (test-event)
  (test-case
    "test-event"
    (define evt (s/event 0 'hello))
    (check-equal? (s/event-stamp evt) 0)
    (check-equal? (s/event-value evt) 'hello)
    )
  )


; Test transformers

(define (test-map)
  (test-case
    "test-map"
    (check-equal?
      (s/map add1 (list (s/event 0 0 ) (s/event 1 1)))
      (list (s/event 0 1) (s/event 1 2))
      )
    )
  )

(define (test-filter)
  (test-case
    "test-filter"
    (check-equal?
      (s/filter odd? (list (s/event 0 0) (s/event 1 1)))
      (list (s/event 1 1))
      )
    )
  )

(define (test-ap)
  (test-case
    "test-ap"
    (check-equal? (s/ap '() '()) '())
    ; test empty val stream
    (check-equal? (s/ap (list (s/event 0 add1)) '()) '())
    ; test empty fn stream
    (check-equal? (s/ap '() (list (s/event 1 0))) '())
    ; test input streams with single event, varying timestamps
    (check-equal?
      (s/ap (list (s/event 0 add1)) (list (s/event 1 0)))
      (list (s/event 1 1)))
    (check-equal?
      (s/ap (list (s/event 1 add1)) (list (s/event 0 0)))
      (list (s/event 1 1)))
    ; test input streams with single event, same timestamps
    (check-equal?
      (s/ap (list (s/event 0 add1)) (list (s/event 0 0)))
      (list (s/event 0 1)))
    (check-equal?
      (s/ap (list (s/event 1 add1)) (list (s/event 1 0)))
      (list (s/event 1 1)))
    ; test input streams with two events, varying timestamps
    (check-equal?
      (s/ap
        (list (s/event 0 add1) (s/event 2 sub1))
        (list (s/event 1 0))
        )
      (list (s/event 1 1) (s/event 2 -1))
      )
    (check-equal?
      (s/ap
        (list (s/event 1 add1))
        (list (s/event 0 0) (s/event 2 1))
        )
      (list (s/event 1 1) (s/event 2 2))
      )
    (check-equal?
      (s/ap
        (list (s/event 0 add1) (s/event 2 sub1))
        (list (s/event 1 0) (s/event 3 1))
        )
      (list (s/event 1 1) (s/event 2 -1) (s/event 3 0))
      )
    (check-equal?
      (s/ap
        (list (s/event 1 add1) (s/event 3 sub1))
        (list (s/event 0 0) (s/event 2 1))
        )
      (list (s/event 1 1) (s/event 2 2) (s/event 3 0))
      )
    ; test input streams with two events, same timestamps
    (check-equal?
      (s/ap
        (list (s/event 1 add1) (s/event 2 sub1))
        (list (s/event 1 0))
        )
      (list (s/event 1 1) (s/event 2 -1))
      )
    (check-equal?
      (s/ap
        (list (s/event 1 add1))
        (list (s/event 1 0) (s/event 2 1))
        )
      (list (s/event 1 1) (s/event 2 2))
      )
    (check-equal?
      (s/ap
        (list (s/event 1 add1) (s/event 2 sub1))
        (list (s/event 1 0) (s/event 3 1))
        )
      (list (s/event 1 1) (s/event 2 -1) (s/event 3 0))
      )
    (check-equal?
      (s/ap
        (list (s/event 1 add1) (s/event 3 sub1))
        (list (s/event 1 0) (s/event 2 1))
        )
      (list (s/event 1 1) (s/event 2 2) (s/event 3 0))
      )
    (check-equal?
      (s/ap
        (list (s/event 1 add1) (s/event 2 sub1))
        (list (s/event 1 0) (s/event 2 1))
        )
      (list (s/event 1 1) (s/event 2 0))
      )
    (check-equal?
      (s/ap
        (list (s/event 1 add1) (s/event 2 sub1) (s/event 3 add1))
        (list (s/event 1 0) (s/event 2 1))
        )
      (list (s/event 1 1) (s/event 2 0) (s/event 3 2))
      )
    (check-equal?
      (s/ap
        (list (s/event 1 add1) (s/event 2 sub1))
        (list (s/event 1 0) (s/event 2 1) (s/event 3 2))
        )
      (list (s/event 1 1) (s/event 2 0) (s/event 3 1))
      )
    (check-equal?
      (s/ap
        (list (s/event 1 add1) (s/event 2 sub1) (s/event 3 identity))
        (list (s/event 1 0) (s/event 2 1) (s/event 3 2))
        )
      (list (s/event 1 1) (s/event 2 0) (s/event 3 2))
      )
    )
  )


; Test sources

(define (test-from-diagram)
  (test-case
    "test-from-diagram"
    (define outstream1 (s/from-diagram "1234"))
    (define expected1
      (list (s/event 0 "1") (s/event 20 "2") (s/event 40 "3") (s/event 60 "4")))
    (check-equal? outstream1 expected1)
    (define outstream2 (s/from-diagram "5--6"))
    (define expected2 (list (s/event 0 "5") (s/event 60 "6")))
    (check-equal? outstream2 expected2)
    )
  )


; Test interpreters

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
    (define inputs (list
      (list (s/event 20 0) (s/event 40 1))
      ))
    (define output (s/program-interpret prog inputs))
    (logger-debug "output:")
    (logger-debug output)
    (check-equal? output (list (s/event 20 1)))

    (define numinputs2 2)
    (define prog2
      (l/program
        numinputs2
        (list
          (l/mapTo 1 (l/register 0))
          (l/mapTo -1 (l/register 1))
          (l/merge (l/register 2) (l/register 3))
          (l/scan + 0 (l/register 4))
          )))
    (define inputs2
      (list
        (list (s/event 20 #t) (s/event 60 #t))
        (list (s/event 40 #f) (s/event 80 #f))
        ))

    (check-equal?
      (s/program-interpret prog2 inputs2)
      (list (s/event 20 1) (s/event 40 0) (s/event 60 1) (s/event 80 0))
      )
    )
  )


; Test solver-aided programming

(current-bitwidth #f)
(define-symbolic x y integer?)

(define (test-solve)
  (test-case
    "test-solve"
    (define inst (l/map add1 (list (s/event x y))))
    (define sol
      (time
        (solve
          (assert (equal? (s/instruction-interpret inst '()) (list (s/event 0 2))))
          )
        )
      )
    (check-true (sat? sol))
    (check-equal? (evaluate (list x y) sol) (list 0 1))

    (define inst2
      (if b
        (l/map add1 (list (s/event x y)))
        (l/filter odd? (list (s/event x y)))
        )
      )
    (define sol2
      (time
        (solve
          (assert (equal? (s/instruction-interpret inst2 '()) (list (s/event 0 2))))
          )
        )
      )
    (check-true (sat? sol2))
    (check-equal? (evaluate (list b x y) sol2) (list #t 0 1))
    )
  )

(define-symbolic b boolean?)

(define (test-angexe)
  (test-case
    "test-angexe"
    (define numinputs 2)
    (define spec
      (l/program
        numinputs
        (list (l/mapTo 1 (l/register 0))
          (l/mapTo -1 (l/register 1))
          (l/merge (l/register 2) (l/register 3))
          (l/scan + 0 (l/register 4))
          )))
    (logger-debug "spec:")
    (logger-debug (l/program->string spec))
    (define sketch
      (??program numinputs (length (l/program-instructions spec))
        (list
          l/merge
          (lambda (x) (l/map add1 x))
          (lambda (x) (l/mapTo (??constant) x))
          (lambda (x) (l/filter odd? x))
          (lambda (x) (l/scan + 0 x))
          )
        ))
    (logger-debug "sketch:")
    (logger-debug (l/program->string sketch))
    (define inputs
      (list
        (list (s/event 20 #t) (s/event 60 #t))
        (list (s/event 40 #f) (s/event 80 #f))
        ))
    (logger-debug "inputs:")
    (logger-debug inputs)
    (logger-debug "(s/program-interpret spec inputs):")
    (logger-debug (s/program-interpret spec inputs))

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
    (logger-debug "angexe result:")
    (logger-debug (l/program->string result))
    (check-equal?
      (s/program-interpret result inputs)
      (list (s/event 20 1) (s/event 40 0) (s/event 60 1) (s/event 80 0)))
    )
  )


; Main

(module+ test
  (define/provide-test-suite tvpair-tests
    (test-event)
    (test-map)
    (test-filter)
    (test-ap)
    (test-from-diagram)
    (test-program-interpret)
    (test-solve)
    (test-angexe)
    )
  (run-tests tvpair-tests)
  )
