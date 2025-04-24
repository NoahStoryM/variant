#lang racket/base

(require (for-syntax racket/base syntax/parse))

(provide variant call-with-variant let*-variant)

(define (variant #:tag [tag 0] . value*)
  (if (zero? tag)
      (apply values value*)
      (apply values '(#:tag) (list tag) value*)))

(define (call-with-variant generator receiver)
  (call-with-values
   generator
   (case-λ
     [(kw* kw-arg* . value*)
      (if (equal? kw* '(#:tag))
          (if (zero? (car kw-arg*))
              (apply receiver value*)
              (keyword-apply receiver kw* kw-arg* value*))
          (apply receiver kw* kw-arg* value*))]
     [value* (apply receiver value*)])))

(define-syntax (let*-variant stx)
  (syntax-parse stx
    [(_ () body ...+)
     (syntax/loc stx
       (let () body ...))]
    [(_ ([formals expr]) body ...+)
     (syntax/loc stx
       (call-with-variant
        (lambda () expr)
        (lambda formals body ...)))]
    [(_ ([formals expr] [formals* expr*] ...) body ...+)
     (syntax/loc stx
       (let*-variant ([formals expr])
         (let*-variant ([formals* expr*] ...))
           body ...))]))
