#lang racket/base

(require (for-syntax racket/base) scribble/example)
(provide (all-defined-out))

(define (make-variant-eval) (make-base-eval #:lang 'racket/base '(require variant)))
(define-syntax-rule (variant-examples body ...) (examples #:eval (make-variant-eval) body ...))
