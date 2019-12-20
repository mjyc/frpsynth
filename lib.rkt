#lang rosette

(require rosette/lib/lift
  (prefix-in racket/ (only-in racket string-split))
  )

(provide (all-defined-out))

(define-lift string-split
  [(string? string?) racket/string-split])
