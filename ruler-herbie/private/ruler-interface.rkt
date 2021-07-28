#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path)

(provide generate-rational-rules
         generate-boolean-rules)

(define-runtime-path libruler-herbie-path
  (build-path "../" "target" "release"
              (case (system-type)
                [(windows) "ruler_herbie"]
                [else "libruler_herbie"])))

(define-ffi-definer define-ruler-herbie (ffi-lib libruler-herbie-path))

(define-ruler-herbie generate_rational_rules (_fun _uint _uint _uint _bool -> _pointer))
(define-ruler-herbie generate_boolean_rules (_fun _uint _uint _uint _bool -> _pointer))
(define-ruler-herbie destroy_string (_fun _pointer -> _void))

(define-syntax-rule (define-generator name ffi-name)
  (define (name iters argc fuzzc final?)
    (define ptr (ffi-name iters argc fuzzc final?))
    (define str (cast ptr _pointer _string/utf-8))
    (destroy_string ptr)
    str))

(define-syntax-rule (define-generators [name ffi-name] ...)
  (begin (define-generator name ffi-name) ...))


(define-generators
  [generate-rational-rules generate_rational_rules]
  [generate-boolean-rules generate_boolean_rules])
