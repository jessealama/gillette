#lang racket/base

#|

See https://en.wikipedia.org/wiki/XPath#Examples .

|#

(require rackunit
         xml
         (relative-in "../src/"
                      (file "parameters.rkt")
                      (file "eval.rkt")))

(define data #<<XML
<r>
  <e a="1"/>
  <f a="2" b="1">Text 1</f>
  <f/>
  <g>
    <i c="2">Text 2</i>
    Text 3
    <j>Text 4</j>
</g> </r>
XML
)

(define doc (read-xml (open-input-string data)))

(module+ test
  (parameterize ([current-document doc])
    (check-xdm-equal? (evaluate-xpath '(/ "r" "f" ('text)))
                      (list "Wikipedia" "Wiktionary"))
    (check-= (length (evaluate-xpath '(/ "Wikimedia" (// "editions"))))
             2
             0)
    (check-equal? (evaluate-xpath '(/ "Wikimedia"
                                      "projects"
                                      "project"
                                      "editions"
                                      "edition"
                                      (= #:language "English")
                                      (text)))
                  (list "en.wikipedia.org"
                        "en.wiktionary.org"))
    (check-equal? (evaluate-xpath (/ "Wikimedia"
                                     "projects"
                                     "project"
                                     (= (@ "name") "Wikipedia")
                                     "editions"
                                     "edition"
                                     (text)))
                  (list "en.wikipedia.org"))))
