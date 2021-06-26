#lang racket/base

#|

See https://en.wikipedia.org/wiki/XPath#Examples .

|#

(require rackunit
         xml)

(define data #<<XML
<?xml version="1.0" encoding="utf-8"?>
<Wikimedia>
  <projects>
    <project name="Wikipedia" launch="2001-01-05">
      <editions>
        <edition language="English">en.wikipedia.org</edition>
        <edition language="German">de.wikipedia.org</edition>
        <edition language="French">fr.wikipedia.org</edition>
        <edition language="Polish">pl.wikipedia.org</edition>
        <edition language="Spanish">es.wikipedia.org</edition>
      </editions>
    </project>
    <project name="Wiktionary" launch="2002-12-12">
      <editions>
        <edition language="English">en.wiktionary.org</edition>
        <edition language="French">fr.wiktionary.org</edition>
        <edition language="Vietnamese">vi.wiktionary.org</edition>
        <edition language="Turkish">tr.wiktionary.org</edition>
        <edition language="Spanish">es.wiktionary.org</edition>
      </editions>
    </project>
  </projects>
</Wikimedia>
XML
)

(define doc (read-xml (open-input-string data)))

(module+ test
  (parameterize ([current-document doc])
    (check-equal? (evaluate-xpath (xpath (/ "Wikimedia" "projects" "project" (@ "name"))))
                  (list "Wikipedia" "Wiktionary"))
    (check-= (length (evaluate-xpath (xpath (/ "Wikimedia" (// "editions")))))
             2
             0)
    (check-equal? (evaluate-xpath (xpath (/ "Wikimedia"
                                            "projects"
                                            "project"
                                            "editions"
                                            "edition"
                                            (= (@ "language") "English")
                                            (text))))
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
