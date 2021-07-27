#lang racket

(provide ruler-rule->rule)

(define (egg-expr->expr expr)
  (define datum (read (open-input-string expr)))
  (let loop ([expr datum])
    (match expr
     [(list op args ...) (cons op (map loop args))]
     [(? number?) expr]
     [(? symbol?)
      (define str (symbol->string expr))
      (if (string-prefix? str "?")
          (string->symbol (substring str 1))
          expr)])))

(define (ruler-rule->rule rule)
  (match-define (list input output) (string-split rule " -> "))
  (cons (egg-expr->expr input) (egg-expr->expr output)))
