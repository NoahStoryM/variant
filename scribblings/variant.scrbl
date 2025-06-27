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

@defstruct*[tag ([number natural?]) #:transparent #:extra-constructor-name make-tag]{
A structure type for @racket[tag]s.

@variant-examples[
(tag 1)
(tag 0)
(eval:error (tag -1))
]
}

@defproc[(variant [v any/c] ... [#:tag n natural? 0]) any]{
A @tech{variant}-aware version of @racket[values]. Constructs @tech{tagged values}.
When @racket[n] is @racket[0] (default), returns plain @tech{values}.

@variant-examples[
(variant 1 2 3)
(variant 1 2 3 #:tag 0)
(variant 1 2 3 #:tag 1)
]
}

@defproc[(inclusion [n natural?]) procedure?]{
Adds @racket[n] to the incoming tag and returns a @tech{tagged values}.

@variant-examples[
((inclusion 0) 1 2 3)
((inclusion 1) 1 2 3)
((inclusion 1) 1 2 3 #:tag 1)
]
}

@defproc[(apply/variant [proc procedure?] [v any/c] ... [lst list?] [#:tag n natural? 0]) any]{
A @tech{variant}-aware version of @racket[apply]. It calls @racket[proc] on
@racket[(list* v ... lst)], passing @racket[#:tag n] as a normal keyword
argument when @racket[n] is non-zero.

@variant-examples[
(apply/variant + 1 2 (list 3))
(apply/variant + 1 2 (list 3) #:tag 0)
(eval:error (apply/variant + 1 2 (list 3) #:tag 1))
(apply/variant
 (λ (a b #:tag [n 0])
   (cons (vector a b) n))
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
 (λ (a b #:tag [n 0])
   (cons (vector a b) n)))
(call-with-variant
 (λ () (variant 'a 'b #:tag 1))
 (λ (a b #:tag [n 0])
   (cons (vector a b) n)))
]
}

@defproc[(compose/variant [proc procedure?] ...) procedure?]{
A @tech{variant}-aware version of @racket[compose]. Composes procedures
with @racket[call-with-variant] so that tags are forwarded between them.
The rightmost procedure is applied first.

Calling @racket[(compose/variant)] returns @racket[variant], which acts
as the identity element, so @racket[(compose/variant variant f variant)]
simply yields @racket[f].

@variant-examples[
(define add1-tag (λ (x) (variant (add1 x) #:tag 1)))
(define unwrap (λ (#:tag [n 0] x) (cons x n)))
((compose/variant unwrap add1-tag) 3)
(compose/variant add1-tag variant)
(compose/variant variant add1-tag)
]
}

@defproc[(distributivity/column-major [#:shape shape vector?] [v any/c] ...) any]{
Distributes nested sums over products according to @racket[shape].
Each argument must start with a @racket[tag] (including @racket[(tag 0)])
indicating which option was chosen. The resulting @tech{variant} is tagged
with the combined index in column-major order.

This procedure follows the usual distributive law of multiplication
over addition.  As an illustration:

@centered{
@math{
(a + b + c) × (d + e + f)
≅ a × d + b × d + c × d
+ a × e + b × e + c × e
+ a × f + b × f + c × f
}}

@variant-examples[
(distributivity/column-major #:shape #(3 3) (tag 0) 'a0 'a1 (tag 0) 'd0 'd1)
(distributivity/column-major #:shape #(3 3) (tag 1) 'b0 'b1 (tag 0) 'd0 'd1)
(distributivity/column-major #:shape #(3 3) (tag 2) 'c0 'c1 (tag 0) 'd0 'd1)
(distributivity/column-major #:shape #(3 3) (tag 0) 'a0 'a1 (tag 1) 'e0 'e1)
(distributivity/column-major #:shape #(3 3) (tag 1) 'b0 'b1 (tag 1) 'e0 'e1)
(distributivity/column-major #:shape #(3 3) (tag 2) 'c0 'c1 (tag 1) 'e0 'e1)
(distributivity/column-major #:shape #(3 3) (tag 0) 'a0 'a1 (tag 2) 'f0 'f1)
(distributivity/column-major #:shape #(3 3) (tag 1) 'b0 'b1 (tag 2) 'f0 'f1)
(distributivity/column-major #:shape #(3 3) (tag 2) 'c0 'c1 (tag 2) 'f0 'f1)
]
}

@defproc[(distributivity/row-major [#:shape shape vector?] [v any/c] ...) any]{
Similar to @racket[distributivity/column-major], but the resulting index is
computed in row-major order.
@centered{
@math{
(a + b + c) × (d + e + f)
≅ a × d + a × e + a × f
+ b × d + b × e + b × f
+ c × d + c × e + c × f
}}
@variant-examples[
 (distributivity/row-major #:shape #(3 3) (tag 0) 'a0 'a1 (tag 0) 'd0 'd1)
 (distributivity/row-major #:shape #(3 3) (tag 1) 'b0 'b1 (tag 0) 'd0 'd1)
 (distributivity/row-major #:shape #(3 3) (tag 2) 'c0 'c1 (tag 0) 'd0 'd1)
 (distributivity/row-major #:shape #(3 3) (tag 0) 'a0 'a1 (tag 1) 'e0 'e1)
 (distributivity/row-major #:shape #(3 3) (tag 1) 'b0 'b1 (tag 1) 'e0 'e1)
 (distributivity/row-major #:shape #(3 3) (tag 2) 'c0 'c1 (tag 1) 'e0 'e1)
 (distributivity/row-major #:shape #(3 3) (tag 0) 'a0 'a1 (tag 2) 'f0 'f1)
 (distributivity/row-major #:shape #(3 3) (tag 1) 'b0 'b1 (tag 2) 'f0 'f1)
 (distributivity/row-major #:shape #(3 3) (tag 2) 'c0 'c1 (tag 2) 'f0 'f1)
]
}

@defform[(let*-variant ([kw-formals rhs-expr] ...) body ...+)
         #:grammar
         [(kw-formals (arg ...)
                      (arg ...+ . rest-id)
                      rest-id)
          (arg id
               [id default-expr]
               (code:line #:tag id)
               (code:line #:tag [id default-expr]))]]{
A @tech{variant}-aware version of @racket[let*-values]. Works with @tech{variants}.

@variant-examples[
(let*-variant ([v* (variant 1 2 3)]) v*)
(let*-variant ([(v . v*) (variant 1 2 3)]) (cons v* v))
(let*-variant ([(v . v*) (variant 1 2 3 #:tag 0)]) (cons v* v))
(eval:error (let*-variant ([(v . v*) (variant 1 2 3 #:tag 1)]) (cons v* v)))
(let*-variant ([(#:tag n v . v*)
                (variant 1 2 3 #:tag 1)])
  (cons (cons v* v) n))
(let*-variant ([(#:tag [n 0] v . v*)
                (variant 1 2 3)])
  (cons (cons v* v) n))
(eval:error
 (let*-variant ([(#:tag n v . v*)
                 (variant 1 2 3)])
   (cons (cons v* v) n)))
(eval:error
 (let*-variant ([(#:tag n v . v*)
                 (variant 1 2 3 #:tag 0)])
   (cons (cons v* v) n)))
]
}

@defform[(define-variant kw-formals expr)]{
A @tech{variant}-aware version of @racket[define-values]. Works with @tech{variants}.

@variant-examples[
(let () (define-variant v* (variant 1 2 3)) v*)
(let () (define-variant (v . v*) (variant 1 2 3)) (cons v* v))
(let () (define-variant (v . v*) (variant 1 2 3 #:tag 0)) (cons v* v))
(eval:error (let () (define-variant (v . v*) (variant 1 2 3 #:tag 1)) (cons v* v)))
(let ()
  (define-variant (#:tag n v . v*)
    (variant 1 2 3 #:tag 1))
  (cons (cons v* v) n))
(let ()
  (define-variant (#:tag [n 0] v . v*)
    (variant 1 2 3))
  (cons (cons v* v) n))
(eval:error
 (let ()
   (define-variant (#:tag n v . v*)
     (variant 1 2 3))
   (cons (cons v* v) n)))
(eval:error
 (let ()
   (define-variant (#:tag n v . v*)
     (variant 1 2 3 #:tag 0))
   (cons (cons v* v) n)))
]
}
