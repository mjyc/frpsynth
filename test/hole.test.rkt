#lang rosette/safe

(provide (all-defined-out))

(require rackunit rackunit/text-ui
  "../hole.rkt"
  (prefix-in l/ "../lang.rkt")
  )

(define (test-??program)
  (test-case
    "test-??program"
    (define sketch (??program 2 5
      (list
        (lambda (x) l/mapTo 1)
        )
      ))
    (check-true (l/program? sketch))
    (check-equal? (length (l/program-instructions sketch)) 5)
    )
  )

(module+ test
  (define/provide-test-suite hole-tests
    (test-??program)
    )
  (run-tests hole-tests)
  )
