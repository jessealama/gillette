#lang typed/racket/base

(provide current-node
         current-position
         current-axis)

(require "types.rkt")

(: current-node (Parameter (Option XDMNode)))
(define current-node (make-parameter #f))

(: current-position (Parameter Exact-Positive-Integer))
(define current-position (make-parameter 1))

(: current-axis (Parameter AxisSymbol))
(define current-axis (make-parameter 'child))
