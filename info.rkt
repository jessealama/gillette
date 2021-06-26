#lang info

(define collection "gilette")

(define version "0.1")

(define deps
  '("base"
    "rackunit-lib"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "rackunit-lib"))

(define pkg-desc "Implementation of XPath 3.1")

(define pkg-authors '("jesse@serverracket.com"))

(define scribblings '(("scribblings/gilette.scrbl" ())))
