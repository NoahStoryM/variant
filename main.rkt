#lang racket/base

(require (for-syntax racket/base)
         racket/contract/base)

(provide (contract-out
          [variant (->* () (#:tag natural?) #:rest (listof any/c) any)]
          [apply/variant (->* (procedure?) (#:tag natural?) #:rest (listof any/c) any)]
          [call-with-variant (-> (-> any) procedure? any)])
         (struct-out tag)
         let*-variant)

(define natural? exact-nonnegative-integer?)

(struct tag (number)
  #:transparent
  #:guard
  (λ (n name)
    (unless (natural? n)
      (raise-argument-error name "natural?" n))
    n))

(define (variant #:tag [n 0] . v*)
  (if (zero? n)
      (apply values v*)
      (apply values (tag n) v*)))

(define (apply/variant proc #:tag [n 0] . v*)
  (let ([v* (apply list* v*)])
    (if (zero? n)
        (apply proc v*)
        (apply proc #:tag n v*))))

(define (call-with-variant generator receiver)
  (define receiver*
    (case-λ
      [(t . v*)
       (if (tag? t)
           (apply apply/variant #:tag (tag-number t) (list receiver v*))
           (apply receiver t v*))]
      [v* (apply receiver v*)]))
  (call-with-values generator receiver*))

(define-syntax let*-variant
  (syntax-rules ()
    [(_ () body body* ...)
     (let () body body* ...)]
    [(_ ([formals expr]) body body* ...)
     (call-with-variant
      (lambda () expr)
      (lambda formals body body* ...))]
    [(_ ([formals expr] [formals* expr*] ...) body body* ...)
     (let*-variant ([formals expr])
       (let*-variant ([formals* expr*] ...)
         body body* ...))]))
