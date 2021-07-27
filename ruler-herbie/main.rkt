#lang racket

(require "private/ruler-interface.rkt")

(provide
  (contract-out
   [create-rational-rules (-> exact-positive-integer?
                              exact-positive-integer?
                              exact-positive-integer?
                              boolean?
                              (listof (cons/c expr? expr?)))]))

(define/match (expr? thing)
  [((list elems ...)) (andmap expr? elems)]
  [((? number?)) #t]
  [((? symbol?)) #t])

(define (create-rational-rules iters argc fuzzc final?)
  (generate_rational_rules iters argc fuzzc final?)
  '())
