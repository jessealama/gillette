#lang racket/base

(require racket/contract
         (prefix-in xml: xml)
         (file "structs.rkt"))

(provide attributes
         base-uri
         children
         document-uri
         is-id?
         is-idrefs?
         namespace-nodes
         nilled?
         node-kind
         node-name
         parent
         string-value
         type-name
         typed-value
         unparsed-entity-public-id
         unparsed-entity-system-id)

(define/contract (attributes aNode)
  (node? . -> . (listof attribute-node))
  (list))

(define/contract (base-uri aNode)
  (node? . -> . (or/c #f string?))
  #f)

(define/contract (children aNode)
  (node? . -> . (listof node))
  (node-children aNode))

(define/contract (document-uri aNode)
  (node? . -> . (or/c #f string?))
  #f)

(define/contract (is-id? aNode)
  (node? . -> . boolean?)
  #f)

(define/contract (is-idrefs? aNode)
  (node? . -> . boolean?)
  #f)

(define/contract (namespace-nodes aNode)
  (node? . -> . (listof node?))
  (list))

(define/contract (nilled? aNode)
  (node? . -> . boolean?)
  #f)

(define/contract (node-kind aNode)
  (node? . -> . (one-of/c "attribute"
                          "comment"
                          "document"
                          "element"
                          "namespace"
                          "processing-instruction"
                          "text"))
  "text")

(define/contract (node-name aNode)
  (node? . -> . string?)
  "")

(define/contract (parent aNode)
  (node? . -> . (or/c #f node?))
  (node-parent aNode))

(define/contract (string-value aNode)
  (node? . -> . string?)
  "")

(define/contract (type-name aNode)
  (node? . -> . (or/c #f string?))
  #f)

(define/contract (typed-value aNode)
  (node? . -> . (listof number?))
  (list))

(define/contract (unparsed-entity-public-id aNode entityName)
  (node? string? . -> . (or/c #f string?))
  #f)

(define/contract (unparsed-entity-system-id aNode entityName)
  (node? string? . -> . (or/c #f string?))
  #f)

; (struct defined in xml) (or node? #f) -> node?
(define (xml->node x [parent #f])
  (cond [(xml:document? x)
         (define new-node (document-node parent #f (list)))
         (define kids (map (lambda (n)
                             (xml->node n new-node))
                           (cons (xml:document-element x)
                                 (xml:document-misc x))))
         (set-node-children! new-node kids)
         new-node]
        [(xml:element? x)
         (define new-node (element-node parent
                                        (list) ; will be updated later
                                        (symbol->string (xml:element-name x))
                                        (list)))
         (define attrs (map (lambda (n)
                              (xml->node n new-node))
                            (xml:element-attributes x)))
         (define kids (map (lambda (n)
                             (xml->node n new-node))
                           (xml:element-content x)))
         (set-node-children! new-node kids)
         (set-element-node-attributes! attrs)]
        [(xml:attribute? x)
         (attribute-node parent
                         (list)
                         (symbol->string (xml:attribute-name x))
                         (xml:attribute-value x))]
        [(xml:p-i? x)
         (processing-instruction-node parent
                                      (list)
                                      (xml:p-i-target-name x)
                                      (xml:p-i-instruction x))]
        [(xml:cdata? x)
         (text-node parent
                    (list)
                    (xml:cdata-string x))]
        [(xml:comment? x)
         (comment-node parent
                       (list)
                       (xml:comment-text x))]))
