#lang racket/base

(provide ancestor
         ancestor-or-self
         attribute
         child
         descendant
         descendant-or-self
         following
         following-sibling
         namespace
         parent
         preceding
         preceding-sibling
         self)

(require racket/contract
         racket/list
         (file "structs.rkt")
         (file "interface.rkt"))

(define/contract (ancestor aNode)
  (node? . -> . (listof node?))
  (define p (parent aNode))
  (cond [(node? p)
         (cons p (ancestor p))]
        [else (list)]))

(define/contract (ancestor-or-self aNode)
  (node? . -> . (listof node?))
  (cons aNode (ancestor aNode)))

(define/contract (attribute aNode)
  (node? . -> . (listof node?))
  (cond [(element-node? aNode)
         (element-node-attributes aNode)]
        [else
         (list)]))

(define/contract (child aNode)
  (node? . -> . (listof node?))
  (node-children aNode))

(define/contract (descendant aNode)
  (node? . -> . (listof node?))
  (define kids (node-children aNode))
  (cond [(eq? #f kids)
         (list)]
        [else
         (append kids
                 (apply append (map descendant kids)))]))

(define/contract (descendant-or-self aNode)
  (node? . -> . (listof node?))
  (cons aNode (descendant aNode)))

(define/contract (following-sibling aNode)
  (node? . -> . (listof node?))
  (define p (parent aNode))
  (cond [(node? p)
         (define kids (node-children p))
         (cond [(eq? #f kids)
                (list)]
               [else
                (define i (index-of p aNode))
                (drop kids i)])]
        [else
         (list)]))

(define/contract (following aNode)
  (node? . -> . (listof node?))
  (define sibs (following-sibling aNode))
  (append sibs
          (apply append
                 (map descendant sibs))))

(define/contract (namespace aNode)
  (node? . -> . (listof node?))
  (list))

(define/contract (parent aNode)
  (node? . -> . (listof node?))
  (define p (parent aNode))
  (cond [p (list p)]
        [else (list)]))

(define/contract (preceding-sibling aNode)
  (node? . -> . (listof node?))
  (define p (parent aNode))
  (cond [(node? p)
         (define kids (node-children p))
         (cond [(eq? #f kids) ; shouldn't happen, but here you go:
                (list)]
               [else
                (define i (index-of p aNode))
                (reverse (take kids i))])]
        [else
         (list)]))

(define/contract (preceding aNode)
  (node? . -> . (listof node?))
  (define (keep-going nodes result)
    (cond [(null? nodes)
           result]
          [else
           (define n (car nodes))
           (cond [(member n result)
                  (keep-going (cdr nodes) result)]
                 [else
                  (define p (parent n))
                  (cond [p
                         (keep-going (cons p (cdr nodes))
                                     (cons n result))]
                        [else
                         (keep-going (cdr nodes)
                                     (cons n result))])])]))
  (keep-going (preceding-sibling aNode) (list)))

(define/contract (self aNode)
  (node? . -> . (listof node?))
  (list aNode))
