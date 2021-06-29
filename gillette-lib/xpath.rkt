#lang racket/base

(provide xpath)

(require racket/list
         syntax/parse
         (for-syntax syntax/parse
                     racket/base)
         (file "parameters.rkt")
         (file "types.rkt")
         (file "equality.rkt")
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
  (define node (current-node))
  (cond [(eq? #f node)
         (error "Current node not set (or empty)")]
        [else
         (find-it node)]))

; -> (listof node?)
(define (enumerate-nodes)
  (define n (current-node))
  (case (current-axis)
    ['ancestor
     (axis:ancestor n)]
    ['ancestor-or-self
     (axis:ancestor-or-self n)]
    ['attribute
     (axis:attribute n)]
    ['child
     (axis:child n)]
    ['descendant
     (axis:descendant n)]
    ['descendant-or-self
     (axis:descendant-or-self n)]
    ['following
     (axis:following n)]
    ['following-sibling
     (axis:following-sibling n)]
    ['namespace
     (axis:namespace n)]
    ['parent
     (axis:parent n)]
    ['preceding
     (axis:preceding n)]
    ['preceding-sibling
     (axis:preceding-sibling n)]
    ['self
     (axis:self n)]))

; string? -> (listof node?)
(define (element [name #f])
  (define pred (cond [(string? name)
                      (lambda (n)
                        (and (element-node? n)
                             (string=? name (element-node-name n))))]
                     [else
                      element-node?]))
  (filter pred (enumerate-nodes)))

; string? -> (listof attribute-node?)
(define (attribute name)
  (define nodes (enumerate-nodes))
  (define (do-it nodes)
    (cond [(null? nodes)
           (list)]
          [(and (attribute-node? (car nodes))
                (string=? name (attribute-node-name (car nodes))))
           (list (car nodes))]
          [(element-node? (car nodes))
           (append (do-it (element-node-attributes (car nodes)))
                   (do-it (cdr nodes)))]
          [else
           (do-it (cdr nodes))]))
  (do-it (enumerate-nodes)))

#|

Examples we should handle:

(xpath "A")
(xpath / "A")
(xpath "A" [1])
(xpath // "A")
(xpath // (= #:id (following * #:id)))

|#

(define (take/safe lst n)
  (with-handlers ([exn:fail:contract? (lambda (e) (list))])
    (take lst n)))

(define (drop/safe lst n)
  (with-handlers ([exn:fail:contract? (lambda (e) (list))])
    (drop lst n)))

(define // 42) ; not actually used!

(define following 99)

(define (atomize items)
  (cond [(null? items)
         (list)]
        [else
         (append (car items)
                 (atomize (cdr items)))]))

(define-syntax (xpath stx)
  (define-literal-set xpath-literals
    (/ // * following))
  (syntax-parse stx
    #:literal-sets (xpath-literals)
    [(_ (following a ...))
     #'(parameterize ([current-axis 'following])
         (xpath a ...))]
    [(_ / a ...) ; (xpath / "A")
     #'(parameterize ([current-node (root)])
         (xpath a ...))]
    [(_ // a ...) ; (xpath // "A")
     #'(parameterize ([current-node (root)]
                      [current-axis 'descendant-or-self])
         (xpath a ...))]
    [(_ test:string) ; (xpath "A")
     #'(element test)]
    [(_ test:string [pos:exact-nonnegative-integer]) ; (xpath "A" [1])
     #'(take/safe (drop/safe (element test)
                             (sub1 pos))
                  1)]
    [(_ *)
     #'(element)]
    [(_ * [test])
     #'(filter (xpath test)
               (element))]
    [(_ * a ...)
     #'(atomize
        (for/list ([node (element)])
          (parameterize ([current-node node])
            (xpath a ...))))]
    [(_ attr:keyword)
     (with-syntax [(a (keyword->string (syntax->datum #'attr)))]
       #'(attribute a))]
    [(_ (= x y))
     #'(lambda (n)
         (parameterize ([current-node n])
           (xdm-equal? (xpath x)
                       (xpath y))))]))

(module+ test
  (define test-doc/string #<<DOC
<A>
  <B id="foo">
    <A/>
  </B>
  <C id="foo"><A id="bar"/></C>
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
                  3)
    (check-equal? (length (xpath // * [(= #:id (following * #:id))]))
                  1)))
