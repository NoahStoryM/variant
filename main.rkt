#lang racket/base

(require (for-syntax racket/base syntax/parse)
         racket/contract/base)

(provide (contract-out
          [variant (->* () (#:tag natural?) #:rest (listof any/c) any)]
          [apply/variant (->* (procedure?) (#:tag natural?) #:rest (listof any/c) any)]
          [call-with-variant (-> (-> any) procedure? any)])
         (struct-out tag)
         let*-variant
         define-variant)

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


(begin-for-syntax
  (define-splicing-syntax-class arg
    [pattern id:id #:with stx-id #'id]
    [pattern [id:id default-expr] #:with stx-id #'id]
    [pattern (~seq #:tag id:id) #:with stx-id #'id]
    [pattern (~seq #:tag [id:id default-expr]) #:with stx-id #'id])
  (define-syntax-class kw-formals
    [pattern rest-id:id #:with stx-id* #'(rest-id)]
    [pattern (arg:arg ...+ . rest-id:id) #:with stx-id* #'(arg.stx-id ... rest-id)]
    [pattern (arg:arg ...) #:with stx-id* #'(arg.stx-id ...)]))


(define-syntax let*-variant
  (syntax-parser
    [(_ () body ...+)
     #'(let () body ...)]
    [(_ ([formals:kw-formals expr]) body ...+)
     #'(call-with-variant
        (lambda () expr)
        (lambda formals body ...))]
    [(_ ([formals:kw-formals expr] [formals*:kw-formals expr*] ...) body ...+)
     #'(let*-variant ([formals expr])
         (let*-variant ([formals* expr*] ...)
           body ...))]))

(define-syntax define-variant
  (syntax-parser
    [(_ formals:kw-formals expr)
     #'(define-values formals.stx-id*
         (let*-variant ([formals expr])
           (values . formals.stx-id*)))]))
