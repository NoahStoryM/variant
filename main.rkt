#lang racket/base

(require (for-syntax racket/base)
         #;racket/contract/base)

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
   (λ (kw* kw-arg* . v*)
     (let-values ([(kw* kw-arg*) (filter-keywords kw* kw-arg*)])
       (if (null? kw*)
           (apply values v*)
           (apply values kw* kw-arg* v*))))
   values))

(define apply/variant
  (make-keyword-procedure
   (λ (kw* kw-arg* proc . v*)
     (let-values ([(v*) (apply list* v*)]
                  [(kw* kw-arg*) (filter-keywords kw* kw-arg*)])
       (if (null? kw*)
           (apply proc v*)
           (keyword-apply proc kw* kw-arg* v*))))
   apply))

(define (call-with-variant generator receiver)
  (define receiver*
    (case-λ
      [(kw* kw-arg* . v*)
       (if (and (list? kw*) (list? kw-arg*)
                (= (length kw*) (length kw-arg*))
                (andmap keyword? kw*)
                #;(apply keyword<? kw*))
           (keyword-apply apply/variant kw* kw-arg* (list receiver v*))
           (apply receiver kw* kw-arg* v*))]
      [v* (apply receiver v*)]))
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
