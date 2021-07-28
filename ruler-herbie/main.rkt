#lang racket

(require "private/ruler-interface.rkt"
         "private/rule-traits.rkt"
         "private/rule-cache.rkt")

(provide
  (contract-out
   [rational-rules (-> exact-positive-integer?
                       natural?
                       natural?
                       boolean?
                       (listof rule?))]
   [boolean-rules (-> exact-positive-integer?
                       natural?
                       natural?
                       boolean?
                       (listof rule?))])
  (rename-out
   [clear-cache clear-rule-cache]))

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

;; from <herbie/conversions.rkt
(define/contract (string-replace* str changes)
  (-> string? (listof (cons/c string? string?)) string?)
  (let loop ([str str] [changes changes])
    (match changes
      [(? null?) str]
      [_ (let ([change (car changes)])
           (loop (string-replace str (car change) (cdr change)) (cdr changes)))])))

(define (egg-expr->expr expr [mode 'float])
  (define replace-table `(("~" . ,(if (equal? mode 'float) "neg" "not"))
                          ("&" . "and")
                          ("|" . "or")
                          ("true" . "TRUE")
                          ("false" . "FALSE")))
  (define datum (read (open-input-string (string-replace* expr replace-table))))
  (let loop ([expr datum])
    (match expr
     [(list op args ...) (cons op (map loop args))]
     [(? number?) expr]
     [(? symbol?)
      (define str (symbol->string expr))
      (if (string-prefix? str "?")
          (string->symbol (substring str 1))
          expr)])))

(define (fp-ruler-rule->rules rule)
  (cond
   [(string-contains? rule "<=>")
    (match-define (list input output) (string-split rule " <=> "))
    (define input* (egg-expr->expr input))
    (define output* (egg-expr->expr output))
    (define fpsafe? (is-fpsafe? input* output*))
    (list (list input* output* (is-simplify? input* output*) fpsafe?)
          (list output* input* (is-simplify? output* input*) fpsafe?))]
   [else
    (match-define (list input output) (string-split rule " => "))
    (define input* (egg-expr->expr input))
    (define output* (egg-expr->expr output))
    (define fpsafe? (is-fpsafe? input* output*))
    (list (list input* output* (is-simplify? input* output*) fpsafe?))]))

(define (bool-ruler-rule->rules rule)
  (cond
   [(string-contains? rule "<=>")
    (match-define (list input output) (string-split rule " <=> "))
    (define input* (egg-expr->expr input 'bool))
    (define output* (egg-expr->expr output 'bool))
    (list (list input* output* (is-simplify? input* output*) #t)
          (list output* input* (is-simplify? output* input*) #t))]
   [else
    (match-define (list input output) (string-split rule " => "))
    (define input* (egg-expr->expr input 'bool))
    (define output* (egg-expr->expr output 'bool))
    (list (list input* output* (is-simplify? input* output*) #t))]))


(define (rational-rules iters argc fuzzc final?)
  (define manifest (rule-manifest "rational" iters argc fuzzc final? '()))
  (define cached (read-cache manifest))
  (cond
   [cached
    (printf "Found rational rules in cache\n")
    (rule-manifest-rules cached)]
   [else
    (define rules-str (generate-rational-rules iters argc fuzzc final?))
    (define rules
      (for/fold ([rules '()]) ([str (string-split rules-str "\n")])
        (append rules (fp-ruler-rule->rules str))))
    (write-cache (struct-copy rule-manifest manifest
                               [rules rules]))
    rules]))

(define (boolean-rules iters argc fuzzc final?)
  (define manifest (rule-manifest "boolean" iters argc fuzzc final? '()))
  (define cached (read-cache manifest))
  (cond
   [cached
    (printf "Found boolean rules in cache\n")
    (rule-manifest-rules cached)]
   [else
    (define rules-str (generate-boolean-rules iters argc fuzzc final?))
    (define rules
      (for/fold ([rules '()]) ([str (string-split rules-str "\n")])
        (append rules (bool-ruler-rule->rules str))))
    (write-cache (struct-copy rule-manifest manifest
                               [rules rules]))
    rules]))
