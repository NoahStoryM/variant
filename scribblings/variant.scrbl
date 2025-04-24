#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     variant)
          "utils.rkt")

@title{Tagged Values}
@defmodule[variant #:packages ("variant")]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

This library implements @racket[variant] as @racket[values] tagged with
a natural number.

@section{Variant}

@defproc[(variant [value any/c] ... [#:tag tag 0]) any]{
Constructs tagged @racket[values]. When @racket[tag] is @racket[0] (the default),
returns plain @racket[values].

@variant-examples[
(variant 1 2 3)
(variant 1 2 3 #:tag 0)
(variant 1 2 3 #:tag 1)
]
}

@defproc[(call-with-variant [generator (-> any)] [receiver procedure?]) any]{
Applies @racket[receiver] to the @racket[variant] produced by @racket[generator].

@variant-examples[
(call-with-variant
 (λ () (variant 'a 'b))
 cons)
(call-with-variant
 (λ () (variant 'a 'b #:tag 0))
 cons)
(call-with-variant
 (λ () (variant 'a 'b))
 (λ (a b #:tag [tag 0]) (cons (cons a b) tag)))
(call-with-variant
 (λ () (variant 'a 'b #:tag 1))
 (λ (a b #:tag [tag 0]) (cons (cons a b) tag)))
]
}

@defform[(let*-variant ([kw-formals val-expr] ...) body ...+)]{
Sequential @racket[variant] binding form.

@variant-examples[
(let*-variant ([v* (variant 1 2 3)]) v*)
(let*-variant ([(v . v*) (variant 1 2 3)]) (cons v* v))
(let*-variant ([(#:tag [tag 0] v . v*) (variant 1 2 3)]) (cons v* tag))
(let*-variant ([(#:tag [tag 0] v . v*) (variant 1 2 3 #:tag 1)]) (cons v* tag))
]
}
