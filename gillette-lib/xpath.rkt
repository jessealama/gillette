#lang racket/base

(provide xpath)

(require racket/list
         syntax/parse
         (for-syntax syntax/parse
                     syntax/parse/class/paren-shape
                     racket/base)
         "parameters.rkt"
         "types.rkt"
         "equality.rkt"
         (prefix-in axis: "axes.rkt"))

(module+ test
  (require rackunit
           (prefix-in xml: xml)
           "convert.rkt"))

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

(define (enumerate&filter test)
  (filter (lambda (n)
            (parameterize ([current-node n])
              (test)))
          (enumerate-nodes)))

; string? -> (listof node?)
(define (element [name #f])
  (define pred (cond [(string? name)
                      (lambda (n)
                        (and (element-node? n)
                             (string=? name (element-node-name n))))]
                     [else
                      element-node?]))
  (filter pred (enumerate-nodes)))

; string? -> (xdm-node? -> boolean?)
(define (element-has-name? name)
  (lambda (n)
    (and (element-node? n)
         (string=? name (element-node-name n)))))

(define (text)
  "")

; string? -> (listof attribute-node?)
(define (attribute name)
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

(define (atomize items)
  (cond [(null? items)
         (list)]
        [else
         (append (car items)
                 (atomize (cdr items)))]))

(define-syntax (xpath stx)
  (syntax-parse stx
    [(_ ((~datum following) a ...))
     #'(parameterize ([current-axis 'following])
         (xpath a ...))]
    [(_ (~datum /) a:string) ; (xpath / "A")
     #'(parameterize ([current-node (root)]
                      [current-axis 'child])
         (element a))]
    [(_ (~datum /) a:string b ...) ; (xpath / "A" "B")
     #'(parameterize ([current-node (root)]
                      [current-axis 'child])
         (atomize
          (for/list ([n (element a)])
            (parameterize ([current-node n])
              (xpath b ...)))))]
    [(_ (~datum //) a ...) ; (xpath // "A" "B")
     #'(parameterize ([current-node (root)]
                      [current-axis 'descendant-or-self])
         (xpath a ...))]
    [(_ test:string)
     #'(element test)]
    [(_ test:string a ...)
     #'(atomize
        (for/list ([n (element test)])
          (parameterize ([current-node n])
            (xpath a ...))))]
    [(_ [~brackets pos:exact-nonnegative-integer])
     #'(take/safe (drop/safe (enumerate-nodes)
                             (sub1 pos))
                  1)]
    [(_ [pos:exact-nonnegative-integer] a ...)
     #'(atomize
        (for/list ([n (take/safe (drop/safe (enumerate-nodes)
                                            (sub1 pos))
                                 1)])
          (parameterize ([current-node n])
            (xpath a ...))))]
    [(_ (~datum *))
     #'(element)]
    [(_ (~datum *) [~brackets test])
     #'(filter (xpath test)
               (element))]
    [(_ (~datum *) a ...)
     #'(atomize
        (for/list ([n (element)])
          (parameterize ([current-node n])
            (xpath a ...))))]
    [(_ attr:keyword)
     (with-syntax [(a (keyword->string (syntax->datum #'attr)))]
       #'(attribute a))]
    [(_ (~parens (~datum =) x y))
     #'(lambda (n)
         (parameterize ([current-node n])
           (xdm-equal? (xpath x)
                       (xpath y))))]
    [(_ (~parens (~datum text)))
     #'(text)]))

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
    (check-equal? (length (xpath [1]))
                  1)
    (check-equal? (length (xpath "A"))
                  1)
    (check-equal? (length (xpath "A" "B"))
                  1)
    (check-equal? (length (xpath / "A"))
                  1)
    (check-equal? (length (xpath "A" [1]))
                  1)
    (check-equal? (length (xpath // "A"))
                  3)
    (check-equal? (length (xpath // * [(= #:id (following * #:id))]))
                  1)))
