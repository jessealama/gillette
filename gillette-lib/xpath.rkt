#lang racket/base

(provide xpath)

(require racket/list
         syntax/parse
         (for-syntax syntax/parse
                     racket/base)
         (file "parameters.rkt")
         (file "types.rkt")
         (prefix-in axis: (file "axes.rkt")))

(module+ test
  (require rackunit
           (prefix-in xml: xml)
           (file "convert.rkt")))

; -> element-node?
(define (root)
  (define (find-it n)
    (define p (node-parent n))
    (cond [(eq? #f p) n]
          [else (find-it p)]))
  (find-it (current-node)))

(define (child predicate)
  (define nodes (axis:child (current-node)))
  (filter predicate nodes))

(define (ancestor predicate)
  (filter predicate (axis:ancestor (current-node))))

(define (ancestor-or-self predicate)
  (filter predicate (axis:ancestor-or-self (current-node))))

(define (attribute predicate)
  (filter predicate (axis:attribute (current-node))))

(define (descendant predicate)
  (filter predicate (axis:descendant (current-node))))

(define (descendant-or-self predicate)
  (define nodes (axis:descendant-or-self (current-node)))
  (filter predicate (axis:descendant-or-self (current-node))))

; string? -> (node? -> boolean?)
(define (element name)
  (lambda (n)
    (and (element-node? n)
         (string=? name (element-node-name n)))))

#|

Examples we should handle:

(xpath "A")
(xpath / "A")
(xpath "A" [1])
(xpath // "A")

|#

(define (take/safe lst n)
  (with-handlers ([exn:fail:contract? (lambda (e) (list))])
    (take lst n)))

(define (drop/safe lst n)
  (with-handlers ([exn:fail:contract? (lambda (e) (list))])
    (drop lst n)))

(define // 42) ; not actually used!

(define-syntax (xpath stx)
  (define-literal-set xpath-literals
    (/ // *))
  (syntax-parse stx
    #:literal-sets (xpath-literals)
    [(_ / test:string) ; (xpath / "A")
     #'(parameterize ([current-node (root)])
         (child (element test)))]
    [(_ // test:string) ; (xpath // "A")
     #'(parameterize ([current-node (root)])
         (descendant-or-self (element test)))]
    [(_ test:string) ; (xpath "A")
     #'(child (element test))]
    [(_ test:string [pos:exact-nonnegative-integer]) ; (xpath "A" [1])
     #'(take/safe (drop/safe (child (element test)) (sub1 pos))
                  1)]))

(module+ test
  (define test-doc/string #<<DOC
<A>
  <B>
    <A/>
  </B>
  <C><A/></C>
</A>
DOC
)
  (define test-doc/xml (xml:read-xml/document (open-input-string test-doc/string)))
  (define test-doc/xdm (xml->xdm test-doc/xml))
  (parameterize ([current-node test-doc/xdm])
    (check-equal? (length (xpath "A"))
                  1)
    (check-equal? (length (xpath / "A"))
                  1)
    (check-equal? (length (xpath "A" [1]))
                  1)
    (check-equal? (length (xpath // "A"))
                  3)))
