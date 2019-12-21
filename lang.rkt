#lang rosette/safe

(provide (all-defined-out))

(struct map (a1 a2) #:transparent)
(struct mapTo (a1 a2) #:transparent)
(struct filter (a1 a2) #:transparent)
(struct scan (a1 a2 a3) #:transparent)
(struct merge (a1 a2) #:transparent)

(struct register (idx) #:transparent)
(struct program (numinputs instructions) #:transparent)
