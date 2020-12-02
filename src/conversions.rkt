#lang racket

(require "common.rkt" "interface.rkt" "syntax/rules.rkt" "syntax/syntax.rkt"
         (submod "syntax/rules.rkt" internals) (submod "syntax/syntax.rkt" internals))
(provide generate-conversions get-rewrite-operator *conversions*)

(define *conversions* (make-parameter (make-hash)))

(define/contract (string-replace* str changes)
  (-> string? (listof (cons/c string? string?)) string?)
  (let loop ([str str] [changes changes])
    (match changes
      [(? null?) str]
      [_ (let ([change (car changes)])
           (loop (string-replace str (car change) (cdr change)) (cdr changes)))])))

(define (get-rewrite-operator prec)
  (define replace-table `((" " . "_") ("(" . "") (")" . "")))
  (define prec* (string->symbol (string-replace* (~a prec) replace-table)))
  (define rewrite (sym-append '<- prec*))
  (get-parametric-operator rewrite prec))

(define (generate-conversion prec1 prec2)
  (define replace-table `((" " . "_") ("(" . "") (")" . "")))
  (define prec1* (string->symbol (string-replace* (~a prec1) replace-table))) ; fixed point workaround
  (define prec2* (string->symbol (string-replace* (~a prec2) replace-table)))
  (define-values (repr1 repr2) (values (get-representation prec1) (get-representation prec2)))

  ;; Repr conversions, e.g. repr1->repr2
  (define conv1 (sym-append prec1* '-> prec2*))
  (define conv2 (sym-append prec2* '-> prec1*))

  (unless (hash-has-key? parametric-operators conv1)
    (define impl (compose (representation-bf->repr repr2) (representation-repr->bf repr1)))
    (register-operator! conv1 conv1 (list prec1) prec2  ; fallback implementation
      (list (cons 'fl impl) (cons 'bf identity)
            (cons 'ival identity) (cons 'nonffi impl))))
  
  (unless (hash-has-key? parametric-operators conv2)
    (define impl (compose (representation-bf->repr repr1) (representation-repr->bf repr2)))
    (register-operator! conv2 conv2 (list prec2) prec1  ; fallback implementation
      (list (cons 'fl impl) (cons 'bf identity)
            (cons 'ival identity) (cons 'nonffi impl))))

  ;; Repr rewrites, e.g. <-repr
  (define repr-rewrite1 (sym-append '<- prec1*))
  (define repr-rewrite2 (sym-append '<- prec2*))

  (unless (hash-has-key? parametric-operators repr-rewrite1)
    (register-operator! repr-rewrite1 repr-rewrite1 (list prec1) prec1
      (list (cons 'fl identity) (cons 'bf identity)
            (cons 'ival identity) (cons 'nonffi identity))))

  (unless (hash-has-key? parametric-operators repr-rewrite2)
    (register-operator! repr-rewrite2 repr-rewrite2 (list prec2) prec2
      (list (cons 'fl identity) (cons 'bf identity)
            (cons 'ival identity) (cons 'nonffi identity))))

  ;; Repr rewrite/conversion rules
  (define rulename1 (sym-append 'rewrite '- prec2* '/ prec1*))
  (define rulename2 (sym-append 'rewrite '- prec1* '/ prec2*))
  (define rulename3 (sym-append rulename1 '-simplify))
  (define rulename4 (sym-append rulename2 '-simplify))

  (register-ruleset! rulename1 '(arithmetic) (list (cons 'a prec2))
    (list (list rulename1 'a `(,conv1 (,repr-rewrite1 a)))))

  (register-ruleset! rulename2 '(arithmetic) (list (cons 'a prec1))
    (list (list rulename2 'a `(,conv2 (,repr-rewrite2 a)))))

  (register-ruleset! rulename3 '(arithmetic simplify) (list (cons 'a prec1))
    (list (list rulename3 `(,conv2 (,conv1 a)) 'a)))

  (register-ruleset! rulename4 '(arithmetic simplify) (list (cons 'a prec2))
    (list (list rulename4 `(,conv1 (,conv2 a)) 'a))))

(define (generate-conversions convs)
  (define reprs
    (for/fold ([reprs '()]) ([conv convs])
      (define prec1 (first conv))
      (define prec2 (last conv))
      (generate-conversion prec1 prec2)
      (hash-update! (*conversions*) prec1 (λ (x) (cons prec2 x)) '())
      (hash-update! (*conversions*) prec2 (λ (x) (cons prec1 x)) '())
      (set-union reprs (list (get-representation prec1) (get-representation prec2)))))
  (*needed-reprs* (set-union reprs (*needed-reprs*))))