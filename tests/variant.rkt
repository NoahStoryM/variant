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

(test-case "Test `inclusion'"
  (for* ([i (in-range 5)]
         [n (in-range 10)])
    (check-equal?
     (call-with-values
      (λ ()
        (call-with-variant
         (λ () (variant 1 2 3 #:tag i))
         (inclusion n)))
      list)
     (list (tag (+ i n)) 1 2 3))))

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
  (check-equal? (compose/variant) variant)
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
