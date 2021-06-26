#lang info

(define collection 'multi)

(define build-deps '("scribble-lib"
                     "gillette-lib"
                     "racket-doc"))

(define deps '("base"))

(define update-implies '("gillette-lib"))

(define pkg-desc "Documentation for Gillette")

(define pkg-authors '("jesse@serverracket.com"))

(define version "0.1")
