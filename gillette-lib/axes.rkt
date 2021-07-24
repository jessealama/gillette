#lang typed/racket/base

(provide ancestor
         ancestor/multi

         ancestor-or-self
         ancestor-or-self/multi

         attribute
         attribute/multi

         child
         child/multi

         descendant
         descendant/multi

         descendant-or-self
         descendant-or-self/multi

         following
         following/multi

         following-sibling
         following-sibling/multi

         namespace
         namespace/multi

         parent
         parent/multi

         preceding
         preceding/multi

         preceding-sibling
         preceding-sibling/multi

         self
         self/multi)

(require racket/list
         "types.rkt"
         "accessors.rkt")

(: remove-duplicate-nodes (-> (Listof XDMNode)
                              (Listof XDMNode)))
(define (remove-duplicate-nodes nodes)
  (cond [(null? nodes)
         (list)]
        [(findf (lambda ([n : XDMNode])
                  (eq? n (car nodes)))
                (cdr nodes))
         (remove-duplicate-nodes (cdr nodes))]
        [else
         (cons (car nodes)
               (remove-duplicate-nodes (cdr nodes)))]))

(: ancestor : (-> XDMNode
                  (Listof XDMNode)))
(define (ancestor aNode)
  (define p (parent aNode))
  (cond [(node? p)
         (cons p (ancestor p))]
        [else (list)]))

(: ancestor/multi : (-> (Listof XDMNode)
                        (Listof XDMNode)))
(define (ancestor/multi nodes)
  (remove-duplicate-nodes (append-map ancestor nodes)))

(: ancestor-or-self (-> XDMNode
                        (Listof XDMNode)))
(define (ancestor-or-self aNode)
  (cons aNode (ancestor aNode)))

(: ancestor-or-self/multi (-> (Listof XDMNode)
                              (Listof XDMNode)))
(define (ancestor-or-self/multi nodes)
  (remove-duplicate-nodes (append-map ancestor-or-self nodes)))

(: attribute (-> XDMNode
                 (Listof XDMNode)))
(define (attribute aNode)
  (cond [(element-node? aNode)
         (element-node-attributes aNode)]
        [else
         (list)]))

(: attribute/multi (-> (Listof XDMNode)
                       (Listof XDMNode)))
(define (attribute/multi nodes)
  (remove-duplicate-nodes (append-map attribute nodes)))

(: child (-> XDMNode
             (Listof XDMNode)))
(define (child aNode)
  (node-children aNode))

(: child/multi (-> (Listof XDMNode)
                   (Listof XDMNode)))
(define (child/multi nodes)
  (remove-duplicate-nodes (append-map child nodes)))

(: descendant (-> XDMNode
                  (Listof XDMNode)))
(define (descendant aNode)
  (define kids (node-children aNode))
  (cond [(eq? #f kids)
         (list)]
        [else
         (append kids
                 (apply append (map descendant kids)))]))

(: descendant/multi (-> (Listof XDMNode)
                        (Listof XDMNode)))
(define (descendant/multi nodes)
  (remove-duplicate-nodes (append-map descendant nodes)))

(: descendant-or-self (-> XDMNode
                          (Listof XDMNode)))
(define (descendant-or-self aNode)
  (cons aNode (descendant aNode)))

(: descendant-or-self/multi (-> (Listof XDMNode)
                                (Listof XDMNode)))
(define (descendant-or-self/multi nodes)
  (remove-duplicate-nodes (append-map descendant-or-self nodes)))

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

(: following-sibling/multi (-> (Listof XDMNode)
                               (Listof XDMNode)))
(define (following-sibling/multi nodes)
  (remove-duplicate-nodes (append-map following-sibling nodes)))

(: following (-> XDMNode
                 (Listof XDMNode)))
(define (following aNode)
  (define sibs (following-sibling aNode))
  (append sibs
          (apply append
                 (map descendant sibs))))

(: following/multi (-> (Listof XDMNode)
                       (Listof XDMNode)))
(define (following/multi nodes)
  (remove-duplicate-nodes (append-map following nodes)))

(: namespace (-> XDMNode
                 (Listof XDMNode)))
(define (namespace aNode)
  (list))

(: namespace/multi (-> (Listof XDMNode)
                       (Listof XDMNode)))
(define (namespace/multi nodes)
  (remove-duplicate-nodes (append-map namespace nodes)))

(: parent (-> XDMNode
              (Listof XDMNode)))
(define (parent aNode)
  (define p (node-parent aNode))
  (cond [(eq? #f p)
         (list)]
        [else
         (list p)]))

(: parent/multi (-> (Listof XDMNode)
                    (Listof XDMNode)))
(define (parent/multi nodes)
  (remove-duplicate-nodes (append-map parent nodes)))

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

(: preceding-sibling/multi (-> (Listof XDMNode)
                               (Listof XDMNode)))
(define (preceding-sibling/multi nodes)
  (remove-duplicate-nodes (append-map preceding-sibling nodes)))

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

(: preceding/multi (-> (Listof XDMNode)
                       (Listof XDMNode)))
(define (preceding/multi nodes)
  (remove-duplicate-nodes (append-map preceding nodes)))

(: self (-> XDMNode
            (Listof XDMNode)))
(define (self aNode)
  (list aNode))

(: self/multi (-> (Listof XDMNode)
                  (Listof XDMNode)))
(define (self/multi nodes)
  (remove-duplicate-nodes (append-map self nodes)))
