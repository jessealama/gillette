# Gillette—XPath for Racket

[XPath](https://www.w3.org/TR/2017/REC-xpath-31-20170321/)
is a query language for XML-like data, including
HTML. It's a beautiful query language.

Gillette is an implementation of XPath 3.1 for Racket.

## Internal DSL

Gillette is an internal DSL that you can embed into your
Racket programs when you need to query your data. Convert
your XML (whether that's an
[`xexpr?`](https://docs.racket-lang.org/xml/%3F#%28def._%28%28lib._xml%2Fprivate%2Fxexpr-core..rkt%29._xexpr~3f%29%29)
or an
[xml](https://docs.racket-lang.org/xml/)
[`document?`](https://docs.racket-lang.org/xml/%3F#%28def._%28%28lib._xml%2Fmain..rkt%29._document%29%29)) and
you're ready to roll!

## License

Gillette is available under the terms of the MIT License.

## One last thing

Gillette is named after the city in Wyoming.
