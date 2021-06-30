#lang info

(define collection "gillette")

(define build-deps '("scribble-lib"
                     "gillette-lib"
                     "racket-doc"))

(define deps '("base"))

(define update-implies '("gillette-lib"))

(define pkg-desc "Documentation for Gillette")

(define scribblings '(("gilette.scrbl")))
