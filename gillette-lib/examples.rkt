#lang racket/base

(provide xpath)

(require syntax/parse)

#|

Examples

|#

; /A
(xpath (/ "A"))

; which, in full, would be:
(xpath/full (in-root
             (['child (element "A")])))

; which probably would get turned into
(parameterize ([current-node (root)])
  (find-children (node-has-name? "A")))

; Simpler example:

; A

; ==>
(xpath "A")

; ==>
(xpath/full (['child (element "A")]))

; xexpr? -> (or/c string? #f)
(define (xexpr-node-name xe)
  (match xe
    [(list (symbol? n) _)
     (symbol->string n)]
    [_ #f]))

; string? -> (xdm-item? -> boolean?)
(define (node-has-name? n)
  (lambda (node)
    (cond [(xml:xexpr? n)
           (let ([name (xexpr-node-name node)])
             (and (string? name)
                  (string=? name n)))]
          [(xml:element? node)
           (string=? n (xml:element-name node))]
          [(xml:attribute? node)
           (string=? n (xml:attribute-name node))]
          [(xml:p-i? x)
           (string=? n (xml:p-i-name node))]
          [else #f])))

(define/contract (ancestors aNode)
  (node? . -> . (listof node?))
  (define p (parent aNode))
  (cond [(node? p)
         (cons p (ancestors p))]
        [else (list)]))

(define/contract (descendants aNode)
  (node? . -> . (listof node?))
  (cond [(node-with-children? aNode)
         (define kids (node-with-children-children aNode))
         (append kids
                 (apply append (map descendants kids)))]
        [else
         (list)]))

(define/contract (following-siblings aNode)
  (node? . -> . (listof node?))
  (define p (parent aNode))
  (cond [(node-with-children? p)
         (define kids (node-with-children-children p))
         (define i (index-of p aNode))
         (drop kids i)]
        [else
         (list)]))

(define/contract (following aNode)
  (node? . -> . (listof node?))
  (define sibs (following-siblings aNode))
  (append sibs
          (apply append
                 (map descendants sibs))))

; (xdm-item? -> boolean?) -> (listof xdm-item?)
(define (find-child predicate)
  (filter predicate (children (current-node))))

(define (find-ancestor predicate)
  (filter predicate (ancestors (current-node))))

(define (find-ancestor-or-self predicate)
  (define n (current-node))
  (filter predicate (cons n (ancestors n))))

(define (find-attribute predicate)
  (filter predicate (attributes (current-node))))

(define (find-descendant predicate)
  (filter predicate (descendants (current-node))))

(define (find-descendant-or-self predicate)
  (define n (current-node))
  (filter predicate (cons n (descendants n))))

; @id = following::*[@id]/@id
; ==>
(xpath (= #:id (following (* [#:id] #:id))))

; would turn into

(lambda ()
  (define a (attr-ref "id"))
  (cond [(string? a)
         (with-axis 'following
           (find-nodes (lambda (n)
                         (xdm-equal? a (attr-ref n "id")))))]
        [else (list)]))

; 1. Axis (optional)
; 2. Node test
; 3. Predicate (optional)

(xpath ("step1" "step2" "step3"))

; ==> is a compact form for

(xpath/full (['child (element "step1") #f]
             ['child (element "step2") #f]
             ['child (element "step3") #f]))

; Perhaps the predicates could also be made explicit:

(xpath/full (['child (element "step1") (lambda (n) #t)]
             ['child (element "step2") (lambda (n) #t)]
             ['child (element "step3") (lambda (n) #t)]))

; //books[title="Infinite Jest"]

; could be represented as

(xpath (// "books" [(= #:title "Infinite Jest")]))

; which is a compact form for

(xpath/full (['descendant-or-self (element "books")
                                  (lambda ()
                                    (= (attribute "title") "Infinite Jest"))]))

; 4. A//B/*[position()=1]

(xpath "A" // "B" * (= 1 (position)))

; which is compact notation for

(xpath/full (['child (element "A") [(true)]]
             ['descendant-or-self (element "B") [(true)]]
             ['child (element) [(= 1 (position))]]))

; Another example:

; a[/html/@lang='en'][@href='help.php'][1]/@target

(xpath "a" [(= "en" (/ "html" #:lang))
            (= #:href "help.php")
            (= 1 (position))]
       #:target)

(define-syntax (xpath stx)
  (syntax-parse stx
    [test:string [predicates]]
    [test:keyword
     #'(attribute ,(keyword->string test))]))

; in full could be

(xpath/full (['child (element "a")
                     [(= "en" (in-root (['child (element "html") [(true)]]
                                        ['child (attribute "lang") [(true)]])))
                      (= #:href "help.php")
                      (= 1 (position))]]
             ['child (attribute "target") [(true)]]))

; which might be turned into this Racket code:

(lambda (n)
  (with-axis 'child
    (and (node-has-name? "a")
         (xml-equal? "en"
                     (in-root
                      (and (node-has-name? "html")
                           (with-axis 'attribute
                             (node-has-name? "lang")))))
         (xdm-equal? "help.php"
                     (with-axis 'attribute
                       (node-has-node? "href")))
         (xdm-equal? (position) 1)
         (with-axis 'attribute
           (node-has-name? "target")))))

(define-syntax (xpath stx)
  ())
