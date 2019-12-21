#lang rosette/safe

(provide (all-defined-out))

(struct map (a1 a2) #:transparent)
(struct filter (a1 a2) #:transparent)

(struct register (idx) #:transparent)
(struct program (numinputs instructions) #:transparent)
