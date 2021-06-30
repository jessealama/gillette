#lang scribble/manual

@require[@for-label[racket/base
		    gilette]]

@title[#:style "toc"]{Gillette—XPath for Racket}
@author[(author+email "Jesse Alama" "jesse@serverracket.com")]

@defmodule[gilette]

Gilette is an implementation of XPath 3.1 for Racket. Evalute arbitrary XPath 3.1 expressions for XML and JSON documents.

@include-section["limitations.scrbl"]
