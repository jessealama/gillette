#lang racket/base

(provide current-node)

; (or/c #f xdm-item?)
(define current-node (make-parameter #f))
