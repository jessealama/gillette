#lang racket/base

(require racket/match
         (prefix-in xml: xml)
         (file "parameters.rkt"))

(module+ test
  (require rackunit))

(define (/ doc args)
  (log-error "/: doc = ~a args = ~a" doc args)
  (cond [(null? args)
         doc]
        [(string? (car args))
         (cond [(xml:document? doc)
                (/ (xml:document-element doc)
                   args)]
               [(xml:element? doc)
                (cond [(string=? (symbol->string (xml:element-name doc))
                                 (car args))
                       (/ (xml:element-content doc)
                          (cdr args))]
                      [else
                       (list)])]
               [else
                (error (format "/: Cannot handle XML thing ~a (first element of ~a)" (car args) args))])]
        [(procedure? (car args))
         (error "/: Procedures not yet implemented")]
        [else
         (error "/: Cannot handle argument ~a" (car args))]))

(define (evaluate xe)
  (define doc (current-document))
  (unless doc
    (error "Current document is unset"))
  (match xe
    [(list '/ arg ...)
     (/ doc (cdr xe))]
    [else
     (error (format "Cannot evaluate XPath expression ~a" xe))]))

(module+ test
  (let ([doc (xml:read-xml (open-input-string "<hey><p>what?</p></hey>"))]
        [xpath '(/ "hey" "p" (text))])
    (parameterize ([current-document doc])
      (check-equal? (evaluate xpath)
                    (list "what?")))))

; (listof procedure?) -> (listof xdm-item?)
(define (find-nodes tests)
  (cond [(null? tests)
         (list)]
        [else
         (apply append
                (for/list ([node (apply (car tests))])
                  (with-current-node node
                    (find-nodes (cdr tests)))))]))
