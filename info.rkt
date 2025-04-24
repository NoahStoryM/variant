#lang info

(define license 'MIT)
(define collection "variant")
(define version "0.0")

(define pkg-desc "Tagged Values")

(define deps '("base"))
(define build-deps '("scribble-lib"))

(define scribblings '(("scribblings/variant.scrbl")))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
