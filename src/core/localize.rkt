#lang racket

(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt" "../interface.rkt")

(provide localize-error)

(define (repeat c)
  (for/list ([(p e) (in-pcontext (*pcontext*))])
    c))

(define *analyze-cache* (make-hash))
(define *analyze-context* (make-parameter #f))

(define (localize-on-expression expr vars cache repr)
  (hash-ref! cache expr
             (λ ()
                (match expr
                  [(? constant?)
                   (define bf
                     (if (symbol? expr)
                         ((constant-info expr 'bf))
                         (->bf expr repr)))
                   (cons (repeat bf) (repeat 1))]
                  [(? variable?)
                   (cons (map (curryr ->bf (dict-ref (*var-reprs*) expr))
                              (dict-ref vars expr))
                         (repeat 1))]
                  [`(if ,c ,ift ,iff)
                   (let ([exact-ift (car (localize-on-expression ift vars cache repr))]
                         [exact-iff (car (localize-on-expression iff vars cache repr))]
                         [exact-cond (for/list ([(p _) (in-pcontext (*pcontext*))])
				       (apply (eval-prog `(λ ,(map car vars) ,c) 'bf repr) p))])
                     (cons (for/list ([c exact-cond] [t exact-ift] [f exact-iff]) (if c t f))
                           (repeat 1)))]
                  [`(,f ,args ...)
                   (define repr (get-representation (operator-info f 'otype)))
                   (define argreprs
                     (match (operator-info f 'itype)
                       [(? representation-name? type)
                        (map (const (get-representation type)) args)]
                       [(list arg-types ...)
                        (map get-representation arg-types)]))
                   (define <-bf (representation-bf->repr repr))
                   (define arg<-bfs (map representation-bf->repr argreprs))

                   (define argexacts
                     (flip-lists
                      (for/list ([arg args] [repr argreprs])
                        (car (localize-on-expression arg vars cache repr)))))
                   (define argapprox
                     (for/list ([pt argexacts])
                       (for/list ([val pt] [arg<-bf arg<-bfs])
                         (arg<-bf val))))

                   (define exact (map (curry apply (operator-info f 'bf)) argexacts))
                   (define approx (map (curry apply (operator-info f 'fl)) argapprox))
                   (cons exact (map (λ (ex ap) (ulp-difference (<-bf ex) ap repr))
                                    exact approx))]))))

(register-reset
 (λ ()
  (*analyze-context* (*pcontext*))
  (hash-clear! *analyze-cache*)))

(define (localize-error prog repr)
  (define varmap (map cons (program-variables prog)
		      (flip-lists (for/list ([(p e) (in-pcontext (*pcontext*))])
				    p))))
  (define cache
    (if (eq? (*analyze-context*) (*pcontext*))
        *analyze-cache*
        (make-hash)))
  (define expr->loc (location-hash prog))

  (localize-on-expression (program-body prog) varmap cache repr)

  (define locs
    (reap [sow]
          (for ([(expr locs) (in-hash expr->loc)]
                #:when (hash-has-key? cache expr))
            (define err (cdr (hash-ref cache expr)))
            (when (ormap (curry < 1) err)
              (for-each (compose sow (curry cons err)) locs)))))

  (list
    (take-up-to
     (sort locs > #:key (compose errors-score car))
     (*localize-expressions-limit*))
    (take-up-to
     (sort locs < #:key (compose errors-score car))
     (*localize-expressions-limit*))))
