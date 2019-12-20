#lang rosette/safe

(require rackunit rackunit/text-ui
 (prefix-in s/ "../../streams/tvpair.rkt")
 (prefix-in l/ "../../lang.rkt"))

(provide (all-defined-out))

(define (test-event)
  (test-case
    "test-event"
    (define evt (s/event 0 'hello))
    (check-equal? (s/event-stamp evt) 0)
    (check-equal? (s/event-value evt) 'hello)
    )
  )

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
    (check-equal? (s/ap (list (s/event 0 add1)) '()) '())
    (check-equal? (s/ap '() (list (s/event 1 add1))) '())
    (check-equal?
      (s/ap (list (s/event 0 add1)) (list (s/event 1 0)))
      (list (s/event 1 1)))
    (check-equal?
      (s/ap (list (s/event 1 add1)) (list (s/event 0 0)))
      (list (s/event 1 1)))
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
    )
  )

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

(current-bitwidth #f)
(define-symbolic x y integer?)

(define (test-solve)
  (test-case
    "test-solve"
    (define prog (l/map add1 (list (s/event x y))))
    (define sol (solve
      (assert (equal? (s/interpret prog) (list (s/event 0 2))))))
    (check-true (sat? sol))
    (check-equal? (evaluate (list x y) sol) (list 0 1))
    )
  )

(define-symbolic b boolean?)

(define (test-angexe)
  (test-case
    "test-angexe"
    (define prog
      (if b
        (l/map add1 (list (s/event x y)))
        (l/filter odd? (list (s/event x y)))
        )
      )
    (define sol (solve
      (assert (equal? (s/interpret prog) (list (s/event 0 2))))))
    (check-true (sat? sol))
    (check-equal? (evaluate (list b x y) sol) (list #t 0 1))
    )
  )

(module+ test
  (define/provide-test-suite tvpair-tests
    (test-event)
    (test-map)
    (test-filter)
    (test-ap)
    (test-from-diagram)
    (test-solve)
    (test-angexe)
    )
  (run-tests tvpair-tests)
  )
