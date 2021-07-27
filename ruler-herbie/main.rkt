#lang racket

(require "private/ruler-interface.rkt"
         "private/ruler-fpsafe.rkt")

(provide
  (contract-out
   [rational-rules (-> exact-positive-integer?
                       natural?
                       natural?
                       boolean?
                       (listof rule?))]))

(define/match (expr? thing)
  [((list elems ...)) (andmap expr? elems)]
  [((? number?)) #t]
  [((? symbol?)) #t])

(define (rule? lst)
  (and (= (length lst) 4)
       (expr? (first lst))
       (expr? (second lst))
       (boolean? (third lst))
       (boolean? (fourth lst))))

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

(define/match (expr-size expr)
  [((list elems ...)) (apply + (map expr-size elems))]
  [(_) 1])

(define (simplify-rule? input output)
  (<= (expr-size output) (expr-size input)))

(define (ruler-rule->rules rule)
  (cond
   [(string-contains? rule "<=>")
    (match-define (list input output) (string-split rule " <=> "))
    (define input* (egg-expr->expr input))
    (define output* (egg-expr->expr output))
    (define fpsafe? (is-fpsafe? input* output*))
    (list (list input* output* (simplify-rule? input* output*) fpsafe?)
          (list output* input* (simplify-rule? output* input*) fpsafe?))]
   [else
    (match-define (list input output) (string-split rule " => "))
    (define input* (egg-expr->expr input))
    (define output* (egg-expr->expr output))
    (define fpsafe? (is-fpsafe? input* output*))
    (list (list input* output* (simplify-rule? input* output*) fpsafe?))]))

(define (rational-rules iters argc fuzzc final?)
  (define rules-str (generate-rational-rules iters argc fuzzc final?))
  (for/fold ([rules '()]) ([str (string-split rules-str "\n")])
    (append rules (ruler-rule->rules str))))
