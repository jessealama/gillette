#lang racket/base

(require racket/contract
         racket/set
         racket/match
         racket/list
         (prefix-in xml: xml)
         json)

(provide/contract
 [xdm-item? (-> any/c boolean?)]
 [xdm-value? (-> any/c boolean?)]
 [xdm-equal? (-> (or/c xdm-item? xdm-value?)
                 (or/c xdm-item? xdm-value?)
                 boolean?)])

(define (equal-jsexprs? jsexpr1 jsexpr2)
  (cond [(number? jsexpr1)
         (and (number? jsexpr2)
              (= jsexpr1 jsexpr2))]
        [(string? jsexpr1)
         (and (string? jsexpr2)
              (string=? jsexpr2))]
        [(equal? jsexpr1 'null)
         (equal? 'null jsexpr2)]
        [(boolean? jsexpr1)
         (and (boolean? jsexpr2)
              (equal? jsexpr1 jsexpr2))]
        [(list? jsexpr1)
         (and (list? jsexpr2)
              (andmap equal-jsexprs? jsexpr1 jsexpr2))]
        [(hash? jsexpr1)
         (and (hash? jsexpr2)
              (let ([keys1 (hash-keys jsexpr1)]
                    [keys2 (hash-keys jsexpr2)])
                (and (set-equal? (set keys1)
                                 (set keys2))
                     (andmap (lambda (k)
                               (equal-jsexprs? (hash-ref jsexpr1 k)
                                               (hash-ref jsexpr2 k)))
                             keys1))))]))

(define (equal-xexpr-attrs? attrs1 attrs2)
  (cond [(null? attrs1)
         (null? attrs2)]
        [(null? attrs2)
         #f]
        [else
         (define attr-value (car attrs1))
         (define attr (car attr-value))
         (define value (cadr attr-value))
         (define i (index-of attrs2
                             attr-value
                             (lambda (av)
                               (and (equal? attr (car av))
                                    (string=? value (cadr av))))))
         (cond [(eq? #f i) #f]
               [else
                (equal-xexpr-attrs? (cdr attrs1)
                                    (append (take attrs2 i)
                                            (drop attrs2 (add1 i))))])]))

; xexpr? xexpr? -> boolean?
(define (equal-xexprs? xe1 xe2)
  (cond [(string? xe1)
         (and (string? xe2)
              (string=? xe1 xe2))]
        [(symbol? xe1 xe2)
         (and (symbol? xe2)
              (equal? xe1 xe2))]
        [(char? xe1)
         (and (char? xe2)
              (char=? xe1 xe2))]
        [(xml:cdata? xe1)
         (and (xml:cdata? xe2)
              (xml-cdata-equal? xe1 xe2))]
        [(xml:comment? xe1)
         (and (xml:comment? xe2)
              (xml-comments-equal? xe1 xe2))]
        [(xml:p-i? xe1)
         (and (xml:p-i? xe2)
              (xml-processing-instructions-equal? xe1 xe2))]
        [else ; must be a list
         (and (equal-xexpr-attrs? (xexpr-attrs xe1)
                                  (xexpr-attrs xe2))
              (andmap equal-xexprs?
                      (xexpr-children xe1)
                      (xexpr-children xe2)))]))

; external-dtd? external-dtd? -> boolean?
(define (xml-external-dtds-equal? dtd1 dtd2)
  (cond [(xml:external-dtd/public? dtd1)
         (and (xml:external-dtd/public? dtd2)
              (string=? (xml:external-dtd-system dtd1)
                        (xml:external-dtd-system dtd2))
              (string=? (xml:external-dtd/public-public dtd1)
                        (xml:external-dtd/public-public dtd2)))]
        [(xml:external-dtd/system? dtd1)
         (and (xml:external-dtd/system? dtd2)
              (string=? (xml:external-dtd-system dtd1)
                        (xml:external-dtd-system dtd2)))]
        [else
         (string=? (xml:external-dtd-system dtd1)
                   (xml:external-dtd-system dtd2))]))

; document-type? document-type? -> boolean
(define (xml-document-types-equal? doctype1 doctype2)
  (and (eq? (xml:document-type-name doctype1)
            (xml:document-type-name doctype2))
       (xml-external-dtds-equal? (xml:document-type-external doctype1)
                                 (xml:document-type-external doctype2))))

; element? element? -> boolean?
(define (xml-elements-equal? elem1 elem2)
  (and (eq? (xml:element-name elem1)
            (xml:element-name elem2))
       (xdm-equal? (xml:element-attributes elem1)
                   (xml:element-attributes elem2))
       (xdm-equal? (xml:element-content elem1)
                   (xml:element-content elem2))))

; attribute? attribute? -> boolean?
(define (xml-attributes-equal? attr1 attr2)
  (and (eq? (xml:attribute-name attr1)
            (xml:attribute-name attr2))
       (string=? (xml:attribute-value attr1)
                 (xml:attribute-value attr1))))

; processing-instruction? processing-instruction? -> boolean?
(define (xml-processing-instructions-equal? proc1 proc2)
  (and (eq? (xml:p-i-target-name proc1)
            (xml:p-i-target-name proc2))
       (string=? (xml:p-i-instruction proc1)
                 (xml:p-i-instruction proc2))))

; cdata? cdata? -> boolean?
(define (xml-cdata-equal? data1 data2)
  (string=? (xml:cdata-string data1)
            (xml:cdata-string data2)))

; comment? comment? -> boolean?
(define (xml-comments-equal? comment1 comment2)
  (string=? (xml:comment-text comment1)
            (xml:comment-text comment2)))

; prolog? prolog? -> boolean?
(define (xml-prologs-equal? prolog1 prolog2)
  (define (miscs-equal?)
    (and (xdm-equal? (xml:prolog-misc prolog1)
                     (xml:prolog-misc prolog2))
         (xdm-equal? (xml:prolog-misc2 prolog1)
                     (xml:prolog-misc2 prolog2))))
  (define doctype1 (xml:prolog-dtd prolog1))
  (define doctype2 (xml:prolog-dtd prolog2))
  (cond [(xml:document-type? doctype1)
         (and (xml:document-type? doctype2)
              (xml-document-types-equal? doctype1 doctype2)
              (miscs-equal?))]
        [(xml:document-type? doctype2)
         #f]
        [else
         (miscs-equal?)]))

; document? document? -> boolean?
(define (xml-documents-equal? doc1 doc2)
  (and (xml-prologs-equal? (xml:document-prolog doc1)
                           (xml:document-prolog doc2))
       (xml-elements-equal? (xml:document-element doc1)
                            (xml:document-element doc2))
       (xdm-equal? (xml:document-misc doc1)
                   (xml:document-misc doc2))))

; xdm-item? xdm-item? -> boolean?
(define (xdm-items-equal? thing1 thing2)
  (cond [(string? thing1)
         (and (string? thing2)
              (string=? thing1 thing2))]
        [(number? thing1)
         (and (number? thing2)
              (= thing1 thing2))]
        [(boolean? thing1)
         (and (boolean? thing2)
              (equal? thing1 thing2))]
        [(xml:xexpr? thing1)
         (and (xml:xexpr? thing2)
              (equal-xexprs? thing1 thing2))]
        [(jsexpr? thing1)
         (and (jsexpr? thing1)
              (equal-jsexprs? thing1 thing2))]
        [(xml:document? thing1)
         (and (xml:document? thing2)
              (xml-documents-equal? thing1 thing2))]
        [(xml:element? thing1)
         (and (xml:element? thing2)
              (xml-elements-equal? thing1 thing2))]
        [(xml:document-type? thing1)
         (and (xml:document-type? thing2)
              (xml-document-types-equal? thing1 thing2))]
        [(xml:attribute? thing1)
         (and (xml:attribute? thing2)
              (xml-attributes-equal? thing1 thing2))]
        [(xml:p-i? thing1)
         (and (xml:p-i? thing2)
              (xml-processing-instructions-equal? thing1 thing2))]
        [(xml:cdata? thing1)
         (and (xml:cdata? thing2)
              (xml-cdata-equal? thing1 thing2))]
        [(xml:comment? thing1 thing2)
         (and (xml:comment? thing2)
              (xml-comments-equal? thing1 thing2))]))

(define (xdm-equal? thing1 thing2)
  (cond [(xdm-item? thing1)
         (xdm-equal? (list thing1)
                     thing2)]
        [(xdm-item? thing2)
         (xdm-equal? thing1 (list thing2))]
        [else
         (andmap xdm-items-equal? thing1 thing2)]))
