#lang typed/racket/base

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

(require racket/list
         (file "types.rkt")
         (file "accessors.rkt"))

(: ancestor : (-> XDMNode
                  (Listof XDMNode)))
(define (ancestor aNode)
  (define p (parent aNode))
  (cond [(node? p)
         (cons p (ancestor p))]
        [else (list)]))

(: ancestor-or-self (-> XDMNode
                        (Listof XDMNode)))
(define (ancestor-or-self aNode)
  (cons aNode (ancestor aNode)))

(: attribute (-> XDMNode
                 (Listof XDMNode)))
(define (attribute aNode)
  (cond [(element-node? aNode)
         (element-node-attributes aNode)]
        [else
         (list)]))

(: child (-> XDMNode
             (Listof XDMNode)))
(define (child aNode)
  (node-children aNode))

(: descendant (-> XDMNode
                  (Listof XDMNode)))
(define (descendant aNode)
  (define kids (node-children aNode))
  (cond [(eq? #f kids)
         (list)]
        [else
         (append kids
                 (apply append (map descendant kids)))]))

(: descendant-or-self (-> XDMNode
                          (Listof XDMNode)))
(define (descendant-or-self aNode)
  (cons aNode (descendant aNode)))

(: following-sibling (-> XDMNode
                         (Listof XDMNode)))
(define (following-sibling aNode)
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

(: following (-> XDMNode
                 (Listof XDMNode)))
(define (following aNode)
  (define sibs (following-sibling aNode))
  (append sibs
          (apply append
                 (map descendant sibs))))

(: namespace (-> XDMNode
                 (Listof XDMNode)))
(define (namespace aNode)
  (list))

(: parent (-> XDMNode
              (Listof XDMNode)))
(define (parent aNode)
  (define p (node-parent aNode))
  (cond [(eq? #f p)
         (list)]
        [else
         (list p)]))

(: preceding-sibling (-> XDMNode
                         (Listof XDMNode)))
(define (preceding-sibling aNode)
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

(: preceding (-> XDMNode
                 (Listof XDMNode)))
(define (preceding aNode)
  (: keep-going (-> (Listof XDMNode)
                    (Listof XDMNode)
                    (Listof XDMNode)))
  (define (keep-going nodes result)
    (cond [(null? nodes)
           result]
          [else
           (define n (car nodes))
           (cond [(member n result)
                  (keep-going (cdr nodes) result)]
                 [else
                  (define p (node-parent n))
                  (cond [(eq? #f p)
                         (keep-going (cdr nodes)
                                     (cons n result))]
                        [else
                         (keep-going (cons p (cdr nodes))
                                     (cons n result))])])]))
  (keep-going (preceding-sibling aNode) (list)))

(: self (-> XDMNode
            (Listof XDMNode)))
(define (self aNode)
  (list aNode))
