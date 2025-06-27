#lang racket/base

;; This module provides a small library for working with "variants",
;; or tagged collections of values.  A variant behaves like Racket's
;; built-in multiple values, but optionally carries a tag that can be
;; used to distinguish between different variants at runtime.

(require (for-syntax racket/base syntax/parse)
         racket/contract/base)

(provide
  ;; Contracted functions that make up the public API
  (contract-out
   [variant procedure?]
   [apply/variant (->* (procedure?) (#:tag natural?) #:rest (listof any/c) any)]
   [call-with-variant (-> procedure? procedure? any)]
   [compose/variant (->* () () #:rest (listof procedure?) procedure?)]
   [distributivity (->* (#:shape vector?) () #:rest (listof any/c) any)])
  ;; Export the tag structure type and the helper macros
  (struct-out tag)
  let*-variant
  define-variant)

(define natural?
  exact-nonnegative-integer?) ; convenient alias for readability

(struct tag (number)
  ;; Represents the tag attached to a variant.  The guard enforces that
  ;; the tag number is a natural number at construction time.
  #:extra-constructor-name make-tag
  #:transparent
  #:guard
  (λ (n name)
    (unless (natural? n)
      (raise-argument-error name "natural?" n))
    n))

(define (variant #:tag [n 0] . v*)
  ;; Construct a tagged collection of values.  When the tag is zero
  ;; (the default) this behaves just like plain `values`; otherwise a
  ;; `tag` structure is inserted as the first value.
  (if (zero? n)
      (apply values v*)
      (apply values (tag n) v*)))

(define (apply/variant proc #:tag [n 0] . v*)
  ;; Like `apply`, but optionally forwards the `#:tag` keyword to
  ;; `proc` when `n` is non-zero.  The keyword is omitted when `n`
  ;; is 0.
  (let ([v* (apply list* v*)])
    (if (zero? n)
        (apply proc v*)
        (apply proc #:tag n v*))))

(define (call-with-variant generator receiver)
  ;; Variant-aware analogue of `call-with-values`.  The generator
  ;; produces a variant which is then passed to `receiver`.  If the
  ;; values are tagged, `receiver` is called using `apply/variant` so
  ;; that it can accept a matching `#:tag` argument.
  (define receiver*
    (case-λ
      [() (receiver)]
      [(t . v*)
       (if (tag? t)
           (apply/variant receiver #:tag (tag-number t) v*)
           (apply receiver t v*))]))
  (call-with-values generator receiver*))

(define (compose2/variant g f)
  (cond
    [(eq? f variant) g]
    [(eq? g variant) f]
    [else
     (define (composed #:tag [n 0] . a*)
       (call-with-variant (λ () (apply/variant f #:tag n a*)) g))
     composed]))

(define (compose/variant . f*)
  (for/fold ([acc variant])
            ([f (in-list f*)])
    (compose2/variant acc f)))

(define (distributivity #:shape shape . arg*)
  ;; Distribute nested sums over products according to `shape`.
  ;; `shape` is a vector of natural numbers describing the number of
  ;; options for each argument.  Each argument may optionally start with a
  ;; `tag` structure indicating which option was chosen.  The result is a
  ;; single variant tagged with the combined index.
  (define len (vector-length shape))
  (define-values (idx* res*)
    (for/fold ([idx* '()] [res* '()] [arg* arg*]
               #:result
               (if (pair? arg*)
                   (raise-arity-error 'distributivity len)
                   (values (reverse idx*) (reverse res*))))
              ([size (in-vector shape)])
      (let-values ([(idx arg*)
                    (if (and (pair? arg*) (tag? (car arg*)))
                        (values (tag-number (car arg*)) (cdr arg*))
                        (values 0 arg*))])
        (unless (< idx size)
          (raise-argument-error 'distributivity (format "tag < ~a" size) idx))
        (unless (pair? arg*)
          (raise-arity-error 'distributivity len))
        (values (cons idx idx*)
                (cons (car arg*) res*)
                (cdr arg*)))))
  ;; compute combined tag using column-major enumeration
  (define tag-num
    (for/fold ([acc 0] [stride 1] #:result acc)
              ([idx (in-list idx*)]
               [size (in-vector shape)])
      (values (+ acc (* idx stride))
              (* stride size))))
  (apply variant #:tag tag-num res*))

(begin-for-syntax
  ;; Syntax classes used by the macro definitions below.  They parse the
  ;; keyword-formal lists that allow optional `#:tag` arguments.
  (define-splicing-syntax-class arg
    [pattern id:id #:with stx-id #'id]
    [pattern [id:id default-expr] #:with stx-id #'id]
    [pattern (~seq #:tag id:id) #:with stx-id #'id]
    [pattern (~seq #:tag [id:id default-expr]) #:with stx-id #'id])
  (define-syntax-class kw-formals
    ;; Collects the identifiers from a formal list so that the macros can
    ;; produce the corresponding variable bindings.
    [pattern rest-id:id #:with stx-id* #'(rest-id)]
    [pattern (arg:arg ...+ . rest-id:id) #:with stx-id* #'(arg.stx-id ... rest-id)]
    [pattern (arg:arg ...) #:with stx-id* #'(arg.stx-id ...)]))


(define-syntax let*-variant
  ;; A `let*-values` analogue that works with variants.  Each bound
  ;; expression may produce tagged or untagged values and the
  ;; corresponding formals can include an optional `#:tag` binding.
  (syntax-parser
    [(_ () body ...+)
     #'(let () body ...)]
    [(_ ([formals:kw-formals expr]) body ...+)
     #'(call-with-variant
        (λ () expr)
        (λ formals body ...))]
    [(_ ([formals:kw-formals expr] [formals*:kw-formals expr*] ...) body ...+)
     #'(let*-variant ([formals expr])
         (let*-variant ([formals* expr*] ...)
           body ...))]))

(define-syntax define-variant
  ;; Helper for defining variant-aware bindings.  Expands to
  ;; `let*-variant` wrapped in `define-values` so that the identifiers in
  ;; `formals` become top-level definitions.
  (syntax-parser
    [(_ formals:kw-formals expr)
     #'(define-values formals.stx-id*
         (let*-variant ([formals expr])
           (values . formals.stx-id*)))]))
