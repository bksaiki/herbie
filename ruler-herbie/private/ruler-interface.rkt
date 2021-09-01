#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path)

(provide  
  (rename-out 
   [generate_rational_rules generate-rational-rules]
   [generate_boolean_rules generate-boolean-rules]))

(define-runtime-path libruler-herbie-path
  (build-path "../" "target" "release"
              (case (system-type)
                [(windows) "ruler_herbie"]
                [else "libruler_herbie"])))

(define-ffi-definer define-ruler-herbie (ffi-lib libruler-herbie-path))

(define-ruler-herbie generate_rational_rules (_fun _uint _uint _uint _bool -> _void))
(define-ruler-herbie generate_boolean_rules (_fun _uint _uint _uint _bool -> _void))
