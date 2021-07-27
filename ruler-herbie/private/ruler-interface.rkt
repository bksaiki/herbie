#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path)

(provide generate-rational-rules)

(define-runtime-path libruler-herbie-path
  (build-path "../" "target" "release"
              (case (system-type)
                [(windows) "ruler_herbie"]
                [else "libruler_herbie"])))

(define-ffi-definer define-ruler-herbie (ffi-lib libruler-herbie-path))

(define-ruler-herbie generate_rational_rules (_fun _uint _uint _uint _bool -> _pointer))

(define-ruler-herbie destroy_string (_fun _pointer -> _void))

(define (generate-rational-rules iters argc fuzzc final?)
  (define ptr (generate_rational_rules iters argc fuzzc final?))
  (define str (cast ptr _pointer _string/utf-8))
  (destroy_string ptr)
  str)
