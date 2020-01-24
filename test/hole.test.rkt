#lang rosette/safe

(provide (all-defined-out))

(require
  "../hole.rkt"
  (prefix-in l/ "../lang.rkt")
  )

(displayln ??instruction2)

(??program2 2 5 (list
  l/merge
  (lambda (x) (l/map add1 x))
  ; (lambda (x) (l/mapTo (??constant) x))
  (lambda (x) (l/mapTo 1 x))
  (lambda (x) (l/mapTo -1 x))
  (lambda (x) (l/filter odd? x))
  (lambda (x) (l/scan + 0 x))
  ))
