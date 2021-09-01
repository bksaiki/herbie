#lang racket

(require "private/ruler-interface.rkt"
         "private/rule-traits.rkt"
         "private/rule-cache.rkt")

(provide
  (contract-out
   [make-rational-rules (-> exact-positive-integer?
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

(define (egg-expr->expr expr [rt '()])
  (define datum (read (open-input-string (string-replace* expr rt))))
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
  (match-define (ruler-rule lhs rhs bi?) rule)
  (define rt '(("~" . "neg")))
  (define input (egg-expr->expr lhs rt))
  (define output (egg-expr->expr rhs rt))
  (define fpsafe? (is-fpsafe? input output))
  (if bi?
      (list (list input output (is-simplify? input output) fpsafe?)
            (list output input (is-simplify? output input) fpsafe?))
      (list (list input output (is-simplify? input output) fpsafe?))))


(define (bool-ruler-rule->rules rule)
  (match-define (ruler-rule lhs rhs bi?) rule)
  (define rt '(("~" . "not") ("|" . "or") ("&" . "and")
               ("true" . "(TRUE)") ("false" . "(FALSE)")))
  (define input (egg-expr->expr lhs rt))
  (define output (egg-expr->expr rhs rt))
  (if bi?
      (list (list input output (is-simplify? input output) #t)
            (list output input (is-simplify? output input) #t))
      (list (list input output (is-simplify? input output) #t))))


(define (make-rational-rules iters argc fuzzc final?)
  (define manifest (rule-manifest "rational" iters argc fuzzc final? '()))
  (define cached (read-cache manifest))
  (define rules
    (if cached
        (begin0 (rule-manifest-rules cached)
          (eprintf "Found rational rules in cache\n"))
        (begin
          (make-cache-directory)
          (generate-rational-rules iters argc fuzzc final?)
          (let ([cached (read-cache manifest)])
            (rule-manifest-rules cached)))))
  (for/fold ([rules '()]) ([r (in-list rules)])
    (append rules (fp-ruler-rule->rules r))))

(define (boolean-rules iters argc fuzzc final?)
  (define manifest (rule-manifest "boolean" iters argc fuzzc final? '()))
  (define cached (read-cache manifest))
  (define rules
    (if cached
        (begin0 (rule-manifest-rules cached)
          (eprintf "Found boolean rules in cache\n"))
        (begin
          (make-cache-directory)
          (generate-boolean-rules iters argc fuzzc final?)
          (let ([cached (read-cache manifest)])
            (rule-manifest-rules cached)))))
  (for/fold ([rules '()]) ([r (in-list rules)])
    (append rules (bool-ruler-rule->rules r))))


(module+ main
  (define iters 2)
  (define argc 3)
  (define fuzzc 0)
  (define final? #f)
  (command-line
   #:program "ruler"
   #:once-each
   [("--iters") num "Number of iterations"
    (set! iters (string->number num))]
   [("--variables") num "Number of variables"
    (set! argc (string->number num))]
   [("--num-fuzz") num "Number of fuzzing iterations"
    (set! fuzzc (string->number num))]
   [("--do-final-run") "Do final rewrite phase"
    (set! final? #t)]
   #:args (mode)
   (match (string->symbol mode)
    ['rational
      (make-rational-rules iters argc fuzzc final?)
      (void)]
    ['boolean
      (boolean-rules iters argc fuzzc final?)
      (void)]
    ['clear (clear-cache)]
    [_ (error 'main "Unknown (Herbie) domain for Ruler")])))
