#lang racket/base

(provide check-xdm-equal?)

(require rackunit
         (file "../src/xdm.rkt"))

(define (check-xdm-equal? computed expected [message #f])
  (check-true (xdm-equal? computed expected)
              message))
