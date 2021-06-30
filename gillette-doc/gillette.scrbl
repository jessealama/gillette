#lang scribble/manual

@require[@for-label[racket/base
		    gillette-lib]]

@title[#:style "toc"]{Gillette—XPath for Racket}
@author[(author+email "Jesse Alama" "jesse@serverracket.com")]

@defmodule[gillette]

Gilette is an implementation of XPath 3.1 for Racket. Evalute arbitrary XPath 3.1 expressions for XML and JSON documents.

@include-section["limitations.scrbl"]
