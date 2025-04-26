#lang racket/base

(require (for-syntax racket/base)
         racket/contract/base)

(provide #;(contract-out
            [variant (->* () (#:tag natural?) #:rest (listof any/c) any)]
            [apply/variant (->* (procedure?) (#:tag natural?) #:rest (listof any/c) any)]
            [call-with-variant (-> (-> any) procedure? any)])
         variant apply/variant call-with-variant let*-variant)

(define natural? exact-nonnegative-integer?)

(define (filter-keywords kw* kw-arg*)
  (for/foldr ([kw* '()] [kw-arg* '()])
             ([kw (in-list kw*)] [kw-arg (in-list kw-arg*)])
    (if (and (eq? kw '#:tag) (eqv? kw-arg 0))
        (values kw* kw-arg*)
        (values (cons kw kw*) (cons kw-arg kw-arg*)))))

(define variant
  (make-keyword-procedure
   (λ (kw* kw-arg* . value*)
     (let-values ([(kw* kw-arg*) (filter-keywords kw* kw-arg*)])
       (if (null? kw*)
           (apply values value*)
           (apply values kw* kw-arg* value*))))
   values))

(define apply/variant
  (make-keyword-procedure
   (λ (kw* kw-arg* proc . value*)
     (let-values ([(value*) (apply list* value*)]
                  [(kw* kw-arg*) (filter-keywords kw* kw-arg*)])
       (if (null? kw*)
           (apply proc value*)
           (keyword-apply proc kw* kw-arg* value*))))
   apply))

(define (call-with-variant generator receiver)
  (define receiver*
    (case-λ
      [(kw* kw-arg* . value*)
       (if (and (list? kw*) (list? kw-arg*) (andmap keyword? kw*)
                #;(= (length kw*) (length kw-arg*))
                #;(apply keyword<? kw*))
           (keyword-apply apply/variant kw* kw-arg* receiver value* '())
           (apply receiver kw* kw-arg* value*))]
      [value* (apply receiver value*)]))
  (call-with-values generator receiver*))

(define-syntax let*-variant
  (syntax-rules ()
    [(_ () body body* ...)
     (let () body body* ...)]
    [(_ ([formals expr]) body body* ...)
     (call-with-variant
      (λ () expr)
      (λ formals body body* ...))]
    [(_ ([formals expr] [formals* expr*] ...) body body* ...)
     (let*-variant ([formals expr])
       (let*-variant ([formals* expr*] ...)
         body body* ...))]))
