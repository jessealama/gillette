#lang info

(define collection "gilette")

(define version "0.2")

(define deps
  '("base"
    "rackunit-lib"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "rackunit-lib"))

(define pkg-desc "XPath for Racket")

(define pkg-authors '("jesse@serverracket.com"))

(define scribblings '(("scribblings/gilette.scrbl" ())))
