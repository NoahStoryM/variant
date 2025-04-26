#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     variant)
          "utils.rkt")

@title{Tagged Values}
@defmodule[variant #:packages ("variant")]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

@section{Overview}

This package implements @deftech{variant}s (@deftech{tagged values}) as the dual
of Racket's native multiple @deftech{values}, establishing a mathematical
correspondence between programming constructs and set operations:

@itemlist[
  @item{@bold{Product as Untagged Values}

        Racket's @racket[values] corresponds to @deftech{Cartesian product}
        (@deftech{×}), where @racket[(values v ...)] represents an element of
        a product set. The isomorphism @math{A ≅ A × 1} justifies treating
        @racket[v] as @racket[(values v)].}
  @item{@bold{Sum as Tagged Values}

        The @racket[variant] corresponds to @deftech{disjoint union} (@deftech{+}),
        where @racket[(variant #:tag n v ...)] represents an element of a sum
        (coproduct) set. The isomorphism @math{A ≅ A + 0} justifies treating
        @racket[(values v ...)] as @racket[(variant #:tag 0 v ...)].}
]

@section{API Reference}

@defproc[(variant [value any/c] ... [#:tag tag natural? 0]) any]{
A @tech{variant}-aware version of @racket[values]. Constructs @tech{tagged values}.
When @racket[tag] is @racket[0] (default), returns plain @tech{values}.

@variant-examples[
(variant 1 2 3)
(variant 1 2 3 #:tag 0)
(variant 1 2 3 #:tag 1)
]
}

@defproc[(apply/variant [proc procedure?] [value any/c] ... [lst list?] [#:tag tag natural? 0]) any]{
A @tech{variant}-aware version of @racket[apply]. Applies @racket[proc] to
@racket[(list* value ... lst)] with optional @racket[tag]. When @racket[tag] is
@racket[0] (default), behaves like standard @racket[apply].

@variant-examples[
(apply/variant + 1 2 (list 3))
(apply/variant + 1 2 (list 3) #:tag 0)
(eval:error (apply/variant + 1 2 (list 3) #:tag 1))
(apply/variant
 (λ (a b #:tag [tag 0])
   (cons (vector a b) tag))
 (list 1 2)
 #:tag 1)
]
}

@defproc[(call-with-variant [generator (-> any)] [receiver procedure?]) any]{
A @tech{variant}-aware version of @racket[call-with-values]. Applies
@racket[receiver] to the @tech{variant} produced by @racket[generator].

@variant-examples[
(call-with-variant (λ () (variant 'a 'b)) cons)
(call-with-variant (λ () (variant 'a 'b #:tag 0)) cons)
(eval:error (call-with-variant (λ () (variant 'a 'b #:tag 1)) cons))
(call-with-variant
 (λ () (variant 'a 'b))
 (λ (a b #:tag [tag 0])
   (cons (vector a b) tag)))
(call-with-variant
 (λ () (variant 'a 'b #:tag 1))
 (λ (a b #:tag [tag 0])
   (cons (vector a b) tag)))
(call-with-variant
 (λ () (values '(#:a #:b #:tag) '(a b 1) 1 2 3))
 (λ (#:a a #:b b #:tag [tag 0] . v*)
   (cons (vector a b v*) tag)))
(call-with-variant
 (λ () (values '(#:tag #:a #:b) '(0 a b) 1 2 3))
 (λ (#:a a #:b b #:tag [tag 0] . v*)
   (cons (vector a b v*) tag)))
(eval:error
 (call-with-variant
  (λ () (values '(#:tag #:a #:b) '(1 a b) 1 2 3))
  (λ (#:a a #:b b #:tag [tag 0] . v*)
    (cons (vector a b v*) tag))))
]
}

@defform[(let*-variant ([kw-formals rhs-expr] ...) body ...+)
         #:grammar
         [(kw-formals (arg ...)
                      (arg ...+ . rest-id)
                      rest-id)
          (arg id
               [id default-expr]
               (code:line keyword id)
               (code:line keyword [id default-expr]))]]{
A @tech{variant}-aware version of @racket[let*-values]. Works with @tech{variants}.

@variant-examples[
(let*-variant ([v* (variant 1 2 3)]) v*)
(let*-variant ([(v . v*) (variant 1 2 3)]) (cons v* v))
(let*-variant ([(v . v*) (variant 1 2 3 #:tag 0)]) (cons v* v))
(eval:error (let*-variant ([(v . v*) (variant 1 2 3 #:tag 1)]) (cons v* v)))
(let*-variant ([(#:tag tag v . v*)
                (variant 1 2 3 #:tag 1)])
  (cons (cons v* v) tag))
(let*-variant ([(#:tag [tag 0] v . v*)
                (variant 1 2 3)])
  (cons (cons v* v) tag))
(eval:error
 (let*-variant ([(#:tag tag v . v*)
                 (variant 1 2 3)])
   (cons (cons v* v) tag)))
(eval:error
 (let*-variant ([(#:tag tag v . v*)
                 (variant 1 2 3 #:tag 0)])
   (cons (cons v* v) tag)))
(let*-variant ([(#:b b #:a a . v*)
                (values '(#:a #:b #:tag) '(x y 0) 1 2 3)])
  (vector a b v*))
(let*-variant ([(#:b b #:a a . v*)
                (values '(#:tag #:a #:b) '(0 x y) 1 2 3)])
  (vector a b v*))
(eval:error
 (let*-variant ([(#:b b #:a a . v*)
                 (values '(#:tag #:a #:b) '(1 x y) 1 2 3)])
   (vector a b v*)))
(let*-variant ([(#:b b #:a a #:tag t . v*)
                (values '(#:a #:b #:tag) '(x y 8) 1 2 3)])
  (cons (vector a b v*) t))
(eval:error
 (let*-variant ([(#:b b #:a a #:tag t . v*)
                 (values '(#:a #:b #:tag) '(x y 0) 1 2 3)])
   (cons (vector a b v*) t)))
]
}
