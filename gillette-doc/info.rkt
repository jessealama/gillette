#lang info

(define collection "gillette")

(define build-deps '("scribble-lib"
                     "racket-doc"))

(define deps '("base" "gillette-lib"))

(define update-implies '("gillette-lib"))

(define pkg-desc "Documentation for Gillette")

(define scribblings '(("scribblings/gillette.scrbl")))

(define authors '("jesse@serverracket.com"))
