#lang racket/base

#|

See https://en.wikipedia.org/wiki/XPath#Examples .

|#

(require rackunit
         xml
         gillette)

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
  (parameterize ([current-node (xml->xdm doc)])
    (check-equal? (xpath / "Wikimedia" "projects" "project" #:name)
                  (list "Wikipedia" "Wiktionary"))
    (check-= (length (xpath / "Wikimedia" // "editions"))
             2
             0)
    (check-equal? (xpath / "Wikimedia"
                         "projects"
                         "project"
                         "editions"
                         "edition"
                         [(= #:language "English")]
                         (text))
                  (list "en.wikipedia.org"
                        "en.wiktionary.org"))
    (check-equal? (xpath / "Wikimedia"
                         "projects"
                         "project"
                         [(= #:name "Wikipedia")]
                         "editions"
                         "edition"
                         (text))
                  (list "en.wikipedia.org"))))

(xpath /
       "Wikimedia"
       "projects"
       "project"
       "editions"
       "edition"
       [(= #:language "English")]
       (text))

;; ==>

(parameterize ([current-node (root)]
               [current-axis 'child])
  (xpath "Wikimedia"
         "projects"
         "project"
         "editions"
         "edition"
         [(= #:language "English")]
         (text)))

;; ==>

(parameterize ([current-node (root)]
               [current-axis 'child])
  (atomize
   (for/list ([n (elements-with-name "Wikipedia")])
     (parameterize ([current-node n])
       (atomize
        (for/list ([n (elements-with-name "projects")])
          (parameterize ([current-node n])
            (atomize
             (for/list ([n (elements-with-name "project")])
               (parameterize ([current-node n])
                 (atomize
                  (for/list ([n (elements-with-name "editions")])
                    (parameterize ([current-node n])
                      (atomize
                       (for/list ([n (elements-with-name "edition")])
                         (parameterize ([current-node n])
                           (cond [(and (= #:language "English"))
                                  (xpath (text))]
                                 [else
                                  (list)])))))))))))))))))

#;
(parameterize ([current-node (root)]
               [current-axis 'child])
  (nodes-with-name "Wikimedia")
  (nodes-with-name "projects")
  (nodes-with-name "project")
  (nodes-with-name "editions")
  (nodes-with-name "edition")
  (filter-nodes (xdm-equal? (keyword "language")
                            "English"))
  (text))
