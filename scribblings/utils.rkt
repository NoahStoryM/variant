#lang racket/base

;; Utilities used by the documentation.

(require (for-syntax racket/base)
         scribble/example)
(provide (all-defined-out))

;; Create an evaluator that has the `variant` library pre-required.
(define (make-variant-eval)
  (make-base-eval #:lang 'racket/base '(require variant)))

;; Convenience macro for running examples in the docs with that evaluator.
(define-syntax-rule (variant-examples body ...)
  (examples #:eval (make-variant-eval) body ...))
