#lang racket/base

(provide (struct-out node)
         (struct-out attribute-node)
         (struct-out text-node)
         (struct-out namespace-node)
         (struct-out processing-instruction-node)
         (struct-out comment-node)
         (struct-out document-node)
         (struct-out element-node))

(struct node
  (parent children)
  #:mutable)

(struct attribute-node node
  (name value))

(struct text-node node
  (content))

(struct namespace-node node
  (uri))

(struct processing-instruction-node node
  (target instruction))

(struct comment-node node
  (content))

(struct document-node node
  (document-uri))

(struct element-node node
  (name attributes)
  #:mutable)
