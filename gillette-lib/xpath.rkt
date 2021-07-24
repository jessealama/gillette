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
  (define nodes (current-nodes))
  (cond [(eq? #f nodes)
         (define n (current-node))
         (unless n
           (error "Current node not set!"))
         (current-nodes (case (current-axis)
                          ['ancestor (axis:ancestor n)]
                          ['ancestor-or-self (axis:ancestor-or-self n)]
                          ['attribute (axis:attribute n)]
                          ['child (axis:child n)]
                          ['descendant (axis:descendant n)]
                          ['descendant-or-self (axis:descendant-or-self n)]
                          ['following (axis:following n)]
                          ['following-sibling (axis:following-sibling n)]
                          ['namespace (axis:namespace n)]
                          ['parent (axis:parent n)]
                          ['preceding (axis:preceding n)]
                          ['preceding-sibling (axis:preceding-sibling n)]
                          ['self (axis:self n)]))
         (current-nodes)]
        [else
         (case (current-axis)
           ['ancestor (axis:ancestor/multi node)]
           ['ancestor-or-self (axis:ancestor-or-self/multi nodes)]
           ['attribute (axis:attribute/multi node)]
           ['child (axis:child/multi nodes)]
           ['descendant (axis:descendant/multi nodes)]
           ['descendant-or-self (axis:descendant-or-self/multi nodes)]
           ['following (axis:following/multi nodes)]
           ['following-sibling (axis:following-sibling/multi nodes)]
           ['namespace (axis:namespace/multi nodes)]
           ['parent (axis:parent/multi nodes)]
           ['preceding (axis:preceding/multi nodes)]
           ['preceding-sibling (axis:preceding-sibling/multi nodes)]
           ['self (axis:self/multi nodes)])]))

(define (reset-axis)
  (current-axis 'child))

; string? -> void
(define (element [name #f])
  (define pred (cond [(string? name)
                      (lambda (n)
                        (and (element-node? n)
                             (string=? name (element-node-name n))))]
                     [else
                      element-node?]))
  (define nodes (enumerate-nodes))
  (current-nodes (filter pred nodes)))

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

; exact-nonnegative-integer? -> (listof node?)
(define (index pos)
  (take/safe (drop/safe (current-nodes) (sub1 pos))
             1))

(define-syntax (xpath-predicates stx)
  (syntax-parse stx
    [(_)
     #'(current-nodes)]
    [(_ (~parens (~datum =) x y) test ...)
     #'(begin
         (current-nodes
          (filter (lambda (n)
                    (parameterize ([current-node n])
                      (xdm-equal? (xpath x)
                                  (xpath y))))
                  (current-nodes)))
         (xpath-predicates test ...))]))

(define-syntax (xpath stx)
  (syntax-parse stx
    [(_)
     #'(begin0
           (current-nodes)
         (current-nodes #f))]
    [(_ (~parens (~datum following)))
     (error "Step missing after following axis")]
    [(_ (~parens (~datum following) a ...))
     #'(parameterize ([current-axis 'following])
         (xpath a ...))]
    [(_ (~datum /))
     (error "Step missing after /")]
    [(_ (~datum /) a ...) ; (xpath / "A")
     #'(parameterize ([current-node (root)]
                      [current-axis 'child])
         (xpath a ...))]
    [(_ (~datum //))
     (error "Step missing after //")]
    [(_ (~datum //) a ...) ; (xpath // "A" "B")
     #'(parameterize ([current-node (root)]
                      [current-axis 'descendant-or-self])
         (xpath a ...))]
    [(_ test:string a ...)
     #'(begin
         (element test)
         (xpath a ...))]
    [(_ [~brackets pos:exact-nonnegative-integer] a ...)
     #'(begin
         (index pos)
         (xpath a ...))]
    [(_ (~datum *))
     #'(element)]
    [(_ (~datum *) a ...)
     #'(begin
         (element)
         (xpath a ...))]
    [(_ [~brackets test ...] a ...)
     #'(begin
         (xpath-predicates test ...)
         (xpath a ...))]
    [(_ attr:keyword)
     (with-syntax [(a (keyword->string (syntax->datum #'attr)))]
       #'(attribute a))]
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
