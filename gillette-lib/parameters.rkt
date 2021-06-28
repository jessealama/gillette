#lang typed/racket/base

(provide current-node
         current-axis)

(require (file "types.rkt"))

(: current-node (Parameter (Option XDMNode)))
(define current-node (make-parameter #f))

(: current-axis (Parameter AxisSymbol))
(define current-axis (make-parameter 'child))
