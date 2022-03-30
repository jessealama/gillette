#lang scribble/manual

@title{Shortcomings and missing features}

Gillette suffers from some known deficiencies that you may bump into.

@section{XML namespaces are not (really) supported}

Gillete builds on Racket's built-in @racketmodname[xml] library. That library does not support XML namespaces; Gillette inherits that lack of support. XML documents or XPath expressions that make use of namespaces probably won't be handled correctly.
