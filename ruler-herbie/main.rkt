#lang racket

(require "private/ruler-interface.rkt" "private/ruler-expr.rkt")

(provide
  (contract-out
   [rational-rules (-> exact-positive-integer?
                       natural?
                       natural?
                       boolean?
                       (listof (cons/c expr? expr?)))]))

(define/match (expr? thing)
  [((list elems ...)) (andmap expr? elems)]
  [((? number?)) #t]
  [((? symbol?)) #t])

(define (rational-rules iters argc fuzzc final?)
  (define rules-str (generate-rational-rules iters argc fuzzc final?))
  (map ruler-rule->rule (string-split rules-str "\n")))
