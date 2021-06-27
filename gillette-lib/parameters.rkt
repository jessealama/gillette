#lang typed/racket/base

(provide current-node)

(require (file "types.rkt"))

(: current-node (Parameter (Option XDMNode)))
(define current-node (make-parameter #f))
