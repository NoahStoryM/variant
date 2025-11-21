#lang typed/racket/base

(provide (struct-out tag))

(struct (a) tag ([number : (∩ a Natural)])
  ;; Represents the tag attached to a variant.  The guard enforces that
  ;; the tag number is a natural number at construction time.
  #:extra-constructor-name make-tag
  #:transparent
  #:type-name Tag)
