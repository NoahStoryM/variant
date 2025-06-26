#lang racket/base

;; Unit tests for the variant library.

(require rackunit)
(require "../main.rkt")

(displayln "Test `variant.rkt'")

(define-syntax-rule (check-variant= e1 e2)
  ;; Helper macro that compares two variant expressions by expanding
  ;; them with `let*-variant` and comparing both the values and the tag.
  (check-equal?
   (let*-variant ([(#:tag [n 0] . v1) e1]) (cons v1 n))
   (let*-variant ([(#:tag [n 0] . v2) e2]) (cons v2 n))))

(test-case "Test `tag'"
  (for ([n (in-range 10)])
    (check-pred tag? (tag n)))
  (check-exn exn:fail:contract? (λ () (tag -1)))
  (check-exn exn:fail:contract? (λ () (tag "1"))))

(test-case "Test `variant'"
  (check-variant= (variant 1 2 3)         (values 1 2 3))
  (check-variant= (variant 1 2 3 #:tag 0) (values 1 2 3))
  (check-variant= (variant 1 2 3)         (values (tag 0) 1 2 3))
  (check-variant= (variant 1 2 3 #:tag 0) (values (tag 0) 1 2 3))
  (check-variant= (variant 1 2 3 #:tag 1) (values (tag 1) 1 2 3)))

(test-case "Test `apply/variant'"
  (check-eqv? (apply/variant + 1 2 (list 3)) 6)
  (check-eqv? (apply/variant + 1 2 (list 3) #:tag 0) 6)
  (check-exn exn:fail:contract?
             (λ () (apply/variant + 1 2 (list 3) #:tag 1)))
  (check-equal?
   (apply/variant
    (λ (a b #:tag [n 0]) (cons (vector a b) n))
    (list 1 2) #:tag 1)
   '(#(1 2) . 1)))

(test-case "Test `call-with-variant'"
  (check-equal? (call-with-variant (λ () (variant 'a 'b)) cons) '(a . b))
  (check-equal? (call-with-variant (λ () (variant 'a 'b #:tag 0)) cons) '(a . b))
  (check-exn exn:fail:contract?
             (λ () (call-with-variant (λ () (variant 'a 'b #:tag 1)) cons))))

(test-case "Test `compose/variant'"
  (define (add1-tag x) (variant (add1 x) #:tag 1))
  (define (unwrap #:tag [n 0] x) (cons x n))
  (check-variant=
   ((compose/variant) #:tag 2 1 2)
   (variant 1 2 #:tag 2))
  (check-variant=
   ((compose/variant add1-tag values) 3)
   (variant 4 #:tag 1))
  (check-equal?
   ((compose/variant unwrap add1-tag) 3)
   '(4 . 1))
  (define (inc #:tag [n 0] x) (variant (add1 x) #:tag n))
  (define (pair #:tag [n 0] x) (cons x n))
  (check-equal?
   ((compose/variant pair inc) #:tag 5 1)
   '(2 . 5))
  (check-variant=
   ((compose/variant add1-tag variant) 10)
   (add1-tag 10))
  (check-variant=
   ((compose/variant variant add1-tag) 10)
   (add1-tag 10))
  (check-variant=
   ((compose/variant variant add1-tag variant) 10)
   (add1-tag 10)))

(test-case "Test `let*-variant'"
  (check-equal? (let*-variant ([v* (variant 1 2 3)]) v*) '(1 2 3))
  (check-equal? (let*-variant ([(v . v*) (variant 1 2 3)]) (cons v* v))
                (let*-variant ([(v . v*) (variant 1 2 3 #:tag 0)]) (cons v* v)))
  (check-exn exn:fail:contract?
             (λ () (let*-variant ([(v . v*) (variant 1 2 3 #:tag 1)]) (cons v* v))))
  (check-equal? (let*-variant ([(#:tag n v . v*)
                                (variant 1 2 3 #:tag 1)])
                              (cons (cons v* v) n))
                '(((2 3) . 1) . 1))
  (check-equal? (let*-variant ([(#:tag [n 0] v . v*)
                                (variant 1 2 3)])
                              (cons (cons v* v) n))
                '(((2 3) . 1) . 0))
  (check-exn exn:fail:contract?
             (λ () (let*-variant ([(#:tag n v . v*) (variant 1 2 3)]) (cons (cons v* v) n))))
  (check-exn exn:fail:contract?
             (λ () (let*-variant ([(#:tag n v . v*) (variant 1 2 3 #:tag 0)]) (cons (cons v* v) n)))))

(test-case "Test `define-variant'"
  (check-equal? (let () (define-variant v* (variant 1 2 3)) v*) '(1 2 3))
  (check-equal? (let () (define-variant (v . v*) (variant 1 2 3)) (cons v* v))
                (let () (define-variant (v . v*) (variant 1 2 3 #:tag 0)) (cons v* v)))
  (check-exn exn:fail:contract?
             (λ ()
               (define-variant (v . v*) (variant 1 2 3 #:tag 1))
               (cons v* v)))
  (check-equal? (let ()
                  (define-variant (#:tag n v . v*) (variant 1 2 3 #:tag 1))
                  (cons (cons v* v) n))
                '(((2 3) . 1) . 1))
  (check-equal? (let ()
                  (define-variant (#:tag [n 0] v . v*) (variant 1 2 3))
                  (cons (cons v* v) n))
                '(((2 3) . 1) . 0))
  (check-exn exn:fail:contract?
             (λ ()
               (define-variant (#:tag n v . v*) (variant 1 2 3))
               (cons (cons v* v) n)))
(check-exn exn:fail:contract?
             (λ ()
               (define-variant (#:tag n v . v*) (variant 1 2 3 #:tag 0))
               (cons (cons v* v) n))))

(test-case "Test `distributivity'"
  ;; a
  (check-variant=
   (distributivity #:shape #(1) 'a)
   'a)
  (check-variant=
   (distributivity #:shape #(1) (tag 0) 'a)
   'a)

  ;; a + b
  (check-variant=
   (distributivity #:shape #(2) 'a)
   'a)
  (check-variant=
   (distributivity #:shape #(2) (tag 1) 'b)
   (variant #:tag 1 'b))

  ;; a × b
  (check-variant=
   (distributivity #:shape #(1 1) (tag 0) 'a (tag 0) 'b)
   (values 'a 'b))
  (check-variant=
   (distributivity #:shape #(1 1) 'a (tag 0) 'b)
   (values 'a 'b))
  (check-variant=
   (distributivity #:shape #(1 1) (tag 0) 'a 'b)
   (values 'a 'b))
  (check-variant=
   (distributivity #:shape #(1 1) 'a 'b)
   (values 'a 'b))

  ;; a × (b + c)
  ;; ≅ a × b
  ;; + a × c
  (check-variant=
   (distributivity #:shape #(1 2) 'a 'b)
   (values 'a 'b))
  (check-variant=
   (distributivity #:shape #(1 2) 'a (tag 0) 'b)
   (values 'a 'b))
  (check-variant=
   (distributivity #:shape #(1 2) 'a (tag 1) 'c)
   (variant #:tag 1 'a 'c))

  ;; a × (b + c + d)
  ;; ≅ a × b
  ;; + a × c
  ;; + a × d
  (check-variant=
   (distributivity #:shape #(1 3) 'a 'b)
   (values 'a 'b))
  (check-variant=
   (distributivity #:shape #(1 3) 'a (tag 0) 'b)
   (values 'a 'b))
  (check-variant=
   (distributivity #:shape #(1 3) 'a (tag 1) 'c)
   (variant #:tag 1 'a 'c))
  (check-variant=
   (distributivity #:shape #(1 3) 'a (tag 2) 'd)
   (variant #:tag 2 'a 'd))

  ;; (a + b) × c
  ;; ≅ a × c
  ;; + b × c
  (check-variant=
   (distributivity #:shape #(2 1) 'a 'c)
   (values 'a 'c))
  (check-variant=
   (distributivity #:shape #(2 1) (tag 0) 'a 'c)
   (values 'a 'c))
  (check-variant=
   (distributivity #:shape #(2 1) (tag 1) 'b 'c)
   (variant #:tag 1 'b 'c))

  ;; (a + b) × (c + d) ≅ a × c + b × c + a × d + b × d
  (check-variant=
   (distributivity #:shape #(2 2) 'a 'c)
   (values 'a 'c))
  (check-variant=
   (distributivity #:shape #(2 2) (tag 1) 'b 'c)
   (variant #:tag 1 'b 'c))
  (check-variant=
   (distributivity #:shape #(2 2) 'a (tag 1) 'd)
   (variant #:tag 2 'a 'd))
  (check-variant=
   (distributivity #:shape #(2 2) (tag 1) 'b (tag 1) 'd)
   (variant #:tag 3 'b 'd))

  ;; (a + b) × (c + d + e)
  ;; ≅ a × c
  ;; + b × c
  ;; + a × d
  ;; + b × d
  ;; + a × e
  ;; + b × e
  (check-variant=
   (distributivity #:shape #(2 3) 'a 'c)
   (values 'a 'c))
  (check-variant=
   (distributivity #:shape #(2 3) (tag 1) 'b 'c)
   (variant #:tag 1 'b 'c))
  (check-variant=
   (distributivity #:shape #(2 3) 'a (tag 1) 'd)
   (variant #:tag 2 'a 'd))
  (check-variant=
   (distributivity #:shape #(2 3) (tag 1) 'b (tag 1) 'd)
   (variant #:tag 3 'b 'd))
  (check-variant=
   (distributivity #:shape #(2 3) 'a (tag 2) 'e)
   (variant #:tag 4 'a 'e))
  (check-variant=
   (distributivity #:shape #(2 3) (tag 1) 'b (tag 2) 'e)
   (variant #:tag 5 'b 'e))

  ;; (a + b + c) × d
  ;; ≅ a × d
  ;; + b × d
  ;; + c × d
  (check-variant=
   (distributivity #:shape #(3 1) 'a 'd)
   (values 'a 'd))
  (check-variant=
   (distributivity #:shape #(3 1) (tag 1) 'b 'd)
   (variant #:tag 1 'b 'd))
  (check-variant=
   (distributivity #:shape #(3 1) (tag 2) 'c 'd)
   (variant #:tag 2 'c 'd))

  ;; (a + b + c) × (d + e)
  ;; ≅ a × d
  ;; + b × d
  ;; + c × d
  ;; + a × e
  ;; + b × e
  ;; + c × e
  (check-variant=
   (distributivity #:shape #(3 2) 'a 'd)
   (values 'a 'd))
  (check-variant=
   (distributivity #:shape #(3 2) (tag 1) 'b 'd)
   (variant #:tag 1 'b 'd))
  (check-variant=
   (distributivity #:shape #(3 2) (tag 2) 'c 'd)
   (variant #:tag 2 'c 'd))
  (check-variant=
   (distributivity #:shape #(3 2) 'a (tag 1) 'e)
   (variant #:tag 3 'a 'e))
  (check-variant=
   (distributivity #:shape #(3 2) (tag 1) 'b (tag 1) 'e)
   (variant #:tag 4 'b 'e))
  (check-variant=
   (distributivity #:shape #(3 2) (tag 2) 'c (tag 1) 'e)
   (variant #:tag 5 'c 'e))

  ;; (a + b + c) × (d + e + f)
  ;; ≅ a × d
  ;; + b × d
  ;; + c × d
  ;; + a × e
  ;; + b × e
  ;; + c × e
  ;; + a × f
  ;; + b × f
  ;; + c × f
  (check-variant=
   (distributivity #:shape #(3 3) 'a 'd)
   (values 'a 'd))
  (check-variant=
   (distributivity #:shape #(3 3) (tag 1) 'b 'd)
   (variant #:tag 1 'b 'd))
  (check-variant=
   (distributivity #:shape #(3 3) (tag 2) 'c 'd)
   (variant #:tag 2 'c 'd))
  (check-variant=
   (distributivity #:shape #(3 3) 'a (tag 1) 'e)
   (variant #:tag 3 'a 'e))
  (check-variant=
   (distributivity #:shape #(3 3) (tag 1) 'b (tag 1) 'e)
   (variant #:tag 4 'b 'e))
  (check-variant=
   (distributivity #:shape #(3 3) (tag 2) 'c (tag 1) 'e)
   (variant #:tag 5 'c 'e))
  (check-variant=
   (distributivity #:shape #(3 3) 'a (tag 2) 'f)
   (variant #:tag 6 'a 'f))
  (check-variant=
   (distributivity #:shape #(3 3) (tag 1) 'b (tag 2) 'f)
   (variant #:tag 7 'b 'f))
  (check-variant=
   (distributivity #:shape #(3 3) (tag 2) 'c (tag 2) 'f)
   (variant #:tag 8 'c 'f)))
