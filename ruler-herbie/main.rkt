#lang racket

(require "private/ruler-interface.rkt"
         "private/ruler-fpsafe.rkt")

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

(define (egg-expr->expr expr)
  (define datum (read (open-input-string expr)))
  (let loop ([expr datum])
    (match expr
     [(list op args ...)
      (if (equal? op '~)
          (cons 'neg (map loop args))
          (cons op (map loop args)))]
     [(? number?) expr]
     [(? symbol?)
      (define str (symbol->string expr))
      (if (string-prefix? str "?")
          (string->symbol (substring str 1))
          expr)])))

(define (ruler-rule->rules rule)
  (cond
   [(string-contains? rule "<=>")
    (match-define (list input output) (string-split rule " <=> "))
    (define input* (egg-expr->expr input))
    (define output* (egg-expr->expr output))
    (list (cons input* output*) (cons output* input*))]
   [else
    (match-define (list input output) (string-split rule " => "))
    (define input* (egg-expr->expr input))
    (define output* (egg-expr->expr output))
    (list (cons input* output*))]))

(define (rational-rules iters argc fuzzc final?)
  (define rules-str (generate-rational-rules iters argc fuzzc final?))
  (for/fold ([rules '()]) ([str (string-split rules-str "\n")])
    (append rules (ruler-rule->rules str))))
