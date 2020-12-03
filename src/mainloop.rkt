#lang racket

(require "common.rkt" "programs.rkt" "points.rkt" "alternative.rkt" "errors.rkt"
         "timeline.rkt" "syntax/rules.rkt" "syntax/types.rkt" "conversions.rkt"
         "core/localize.rkt" "core/taylor.rkt" "core/alt-table.rkt" "sampling.rkt"
         "core/simplify.rkt" "core/matcher.rkt" "core/regimes.rkt" "interface.rkt")

(provide (all-defined-out))

;; I'm going to use some global state here to make the shell more
;; friendly to interact with without having to store your own global
;; state in the repl as you would normally do with debugging. This is
;; probably a bad idea, and I might change it back later. When
;; extending, make sure this never gets too complicated to fit in your
;; head at once, because then global state is going to mess you up.

(struct shellstate
  (table next-alt locs children gened-series gened-rewrites simplified)
  #:mutable)

(define ^shell-state^ (make-parameter (shellstate #f #f #f #f #f #f #f)))

(define (^locs^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-locs! (^shell-state^) newval))
  (shellstate-locs (^shell-state^)))
(define (^table^ [newval 'none])
  (when (not (equal? newval 'none))  (set-shellstate-table! (^shell-state^) newval))
  (shellstate-table (^shell-state^)))
(define (^next-alt^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-next-alt! (^shell-state^) newval))
  (shellstate-next-alt (^shell-state^)))
(define (^children^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-children! (^shell-state^) newval))
  (shellstate-children (^shell-state^)))

;; Keep track of state for (finish-iter!)
(define (^gened-series^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-gened-series! (^shell-state^) newval))
  (shellstate-gened-series (^shell-state^)))
(define (^gened-rewrites^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-gened-rewrites! (^shell-state^) newval))
  (shellstate-gened-rewrites (^shell-state^)))
(define (^simplified^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-simplified! (^shell-state^) newval))
  (shellstate-simplified (^shell-state^)))

(define *sampler* (make-parameter #f))
(define *cost-limit* (make-parameter #f))

(define (check-unused-variables vars precondition expr)
  ;; Fun story: you might want variables in the precondition that
  ;; don't appear in the `expr`, because that can allow you to do
  ;; non-uniform sampling. For example, if you have the precondition
  ;; `(< x y)`, where `y` is otherwise unused, then `x` is sampled
  ;; non-uniformly (biased toward small values).
  (define used (set-union (free-variables expr) (free-variables precondition)))
  (unless (set=? vars used)
    (define unused (set-subtract vars used))
    (warn 'unused-variable
          "unused ~a ~a" (if (equal? (set-count unused) 1) "variable" "variables")
          (string-join (map ~a unused) ", "))))

(define (alts-for-every-repr altn)
  (define prec (representation-name (*output-repr*)))
  (filter (λ (altn) (program-body (alt-program altn)))
    (for/list ([(k v) (in-hash (*conversions*))]
              #:unless (equal? k prec)
              #:when (set-member? v prec))
      (define rewrite (get-rewrite-operator k))
      (define prog (alt-program altn))
      (define prog* `(λ ,(program-variables prog) (,rewrite ,(program-body prog))))
      (alt (apply-repr-change prog*) 'start '()))))

;; Setting up
(define (setup-prog! prog
                     #:precondition [precondition #f]
                     #:precision [precision 'binary64]
                     #:specification [specification #f]
                     #:cost [cost-limit #f])
  (*output-repr* (get-representation precision))
  (*cost-limit* cost-limit)
  (when (empty? (*needed-reprs*)) ; if empty, probably debugging
    (*needed-reprs* (list (*output-repr*) (get-representation 'bool))))
  (*var-reprs* (map (curryr cons (*output-repr*)) (program-variables prog)))
  (*start-prog* prog)
  (rollback-improve!)
  (define precondition-prog
    (or precondition (list 'λ (program-variables prog) 'TRUE)))
  (check-unused-variables (program-variables prog) (program-body precondition-prog) (program-body prog))

  (debug #:from 'progress #:depth 3 "[1/2] Preparing points")
  ;; If the specification is given, it is used for sampling points
  (timeline-event! 'analyze)
  (*sampler* (make-sampler (*output-repr*) precondition-prog (or specification prog)))
  (timeline-event! 'sample)
  (*pcontext* (prepare-points (or specification prog) precondition-prog (*output-repr*) (*sampler*)))
  (debug #:from 'progress #:depth 3 "[2/2] Setting up program.")
  (define alt (make-alt prog))
  (^table^ (make-alt-table (*pcontext*) alt (*output-repr*)))
  (define alts (alts-for-every-repr alt))
  (^table^ (atab-add-altns (^table^) alts (*output-repr*)))
  alt)

;; Information
(define (list-alts)
  (printf "Key: [.] = done, [>] = chosen\n")
  (let ([ndone-alts (atab-not-done-alts (^table^))])
    (for ([alt (atab-active-alts (^table^))]
	  [n (in-naturals)])
      (printf "~a ~a ~a\n"
       (cond [(equal? alt (^next-alt^)) ">"]
             [(set-member? ndone-alts alt) " "]
             [else "."])
       (~r #:min-width 4 n)
       (program-body (alt-program alt)))))
  (printf "Error: ~a bits\n" (errors-score (atab-min-errors (^table^)))))

(define (add-conversion! prec1 prec2)
  (define single-conv (list (list prec1 prec2)))
  (generate-conversions single-conv))

;; Begin iteration
(define (choose-alt! n)
  (unless (< n (length (atab-active-alts (^table^))))
    (raise-user-error 'choose-alt! "Couldn't select the ~ath alt of ~a (not enough alts)"
                      n (length (atab-active-alts (^table^)))))
  (define-values (picked table*)
    (atab-pick-alt (^table^) #:picking-func (curryr list-ref n) #:only-fresh #f))
  (^next-alt^ picked)
  (^table^ table*)
  (void))

(define (score-alt alt)
  (errors-score (errors (alt-program alt) (*pcontext*) (*output-repr*))))

(define (choose-best-alt!)
  (define-values (picked table*)
    (atab-pick-alt (^table^) #:picking-func (curry argmin score-alt) #:only-fresh #t))
  (^next-alt^ picked)
  (^table^ table*)
  (debug #:from 'pick #:depth 4 "Picked " picked)
  (void))

(define (choose-mult-alts)
  (define altns (atab-not-done-alts (^table^)))
  (define best (argmin score-alt altns))
  (cond
   [(null? altns) '()]
   [(= (length altns) 1)  ; only best
    (list best)]
   [(< (length altns) 5)  ; best, cheapest
    (define simplest (argmin alt-cost altns))
    (list simplest best)]
   [else
    (define simplest (argmin alt-cost altns))
    (define pred (λ (x) (or (alt-equal? x best) (alt-equal? x simplest))))
    (cons simplest
      (let loop ([altns (filter-not pred altns)] [fuel 3])
        (if (zero? fuel)
            (list best)
            (let ([picked (list-ref altns (random 0 (length altns)))])
              (cons picked (loop (filter-not (curry alt-equal? picked) altns)
                                 (- fuel 1)))))))]))

;; Invoke the subsystems individually
(define (localize!)
  (unless (^next-alt^)
    (raise-user-error 'localize! "No alt chosen. Run (choose-best-alt!) or (choose-alt! n) to choose one"))
  (timeline-event! 'localize)
  (define locs (localize-error (alt-program (^next-alt^)) (*output-repr*)))
  (for/list ([(err loc) (in-dict locs)])
    (timeline-push! 'locations
                    (~a (location-get loc (alt-program (^next-alt^))))
                    (errors-score err)))
  (^locs^ (map cdr locs))
  (void))

(define transforms-to-try
  (let ([invert-x (λ (x) `(/ 1 ,x))] [exp-x (λ (x) `(exp ,x))] [log-x (λ (x) `(log ,x))]
	[ninvert-x (λ (x) `(/ 1 (neg ,x)))])
    `((0 ,identity ,identity)
      (inf ,invert-x ,invert-x)
      (-inf ,ninvert-x ,ninvert-x)
      #;(exp ,exp-x ,log-x)
      #;(log ,log-x ,exp-x))))

; taylor uses older format, resugaring and desugaring needed
; not all taylor transforms are valid in a given repr, return false on failure
(define (taylor-expr expr repr vars transformer)
  (define expr* (resugar-program expr repr #:full #f))
  (with-handlers ([exn:fail? (const #f)]) 
    (let ([approx (approximate expr* vars #:transform transformer)])
      (desugar-program approx repr (*var-reprs*) #:full #f))))

(define (taylor-alt altn loc)
  (define expr (location-get loc (alt-program altn)))
  (define repr (location-repr loc (alt-program altn) (*output-repr*) (*var-reprs*)))
  (define vars (free-variables expr))
  (if (or (null? vars) ;; `approximate` cannot be called with a null vars list
          (not (set-member? '(binary64 binary32) ; currently taylor/reduce breaks with posits
                            (repr-of expr (*output-repr*) (*var-reprs*)))))
      (list altn)
      (for/fold ([alts '()] #:result (reverse alts)) ; filter out failed taylor transforms
                ([transform-type transforms-to-try])
        (match-define (list name f finv) transform-type)
        (define transformer (map (const (cons f finv)) vars))
        (define valid? #t)
        (define altn*
          (alt (location-do loc (alt-program altn) 
                            (λ (x) (let ([expr* (taylor-expr x repr vars transformer)])
                                      (unless expr* (set! valid? #f))
                                      expr*)))
              `(taylor ,name ,loc)
              (list altn)))
        (if valid? (cons altn* alts) alts))))

(define (gen-series!)
  (unless (^locs^)
    (raise-user-error 'gen-series! "No locations selected. Run (localize!) or modify (^locs^)"))
  (when (flag-set? 'generate 'taylor)
    (timeline-event! 'series)

    (define series-expansions
      (apply
       append
       (for/list ([location (^locs^)] [n (in-naturals 1)])
         (debug #:from 'progress #:depth 4 "[" n "/" (length (^locs^)) "] generating series at" location)
         (define tnow (current-inexact-milliseconds))
         (begin0
             (taylor-alt (^next-alt^) location)
           (timeline-push! 'times
                           (~a (location-get location (alt-program (^next-alt^))))
                           (- (current-inexact-milliseconds) tnow))))))
    
    (timeline-log! 'inputs (length (^locs^)))
    (timeline-log! 'outputs (length series-expansions))

    (^children^ (append (^children^) series-expansions)))
  (^gened-series^ #t)
  (void))

(define (gen-rewrites!)
  (unless (^locs^)
    (raise-user-error 'gen-rewrites! "No locations selected. Run (localize!) or modify (^locs^)"))

  (timeline-event! 'rewrite)
  (define rewrite (if (flag-set? 'generate 'rr) rewrite-expression-head rewrite-expression))
  (timeline-push! 'method (~a (object-name rewrite)))
  (define altn (alt-add-event (^next-alt^) '(start rm)))

  (define changelists
    (apply append
	   (for/list ([location (^locs^)] [n (in-naturals 1)])
	     (debug #:from 'progress #:depth 4 "[" n "/" (length (^locs^)) "] rewriting at" location)
             (define tnow (current-inexact-milliseconds))
             (define expr (location-get location (alt-program altn)))
             (begin0 (rewrite expr (*output-repr*) #:rules (*rules*) #:root location)
               (timeline-push! 'times (~a expr) (- (current-inexact-milliseconds) tnow))))))

  (define rules-used
    (append-map (curry map change-rule) changelists))
  (define rule-counts
    (for/hash ([rgroup (group-by identity rules-used)])
      (values (rule-name (first rgroup)) (length rgroup))))

  (define (repr-rewrite-alt altn)
    (alt (apply-repr-change (alt-program altn)) (alt-event altn) (alt-prevs altn)))

  (define rewritten
    (filter (λ (altn) (program-body (alt-program altn)))
      (map repr-rewrite-alt
        (for/list ([cl changelists])
          (for/fold ([altn altn]) ([cng cl])
            (alt (change-apply cng (alt-program altn)) (list 'change cng) (list altn)))))))

  (timeline-log! 'inputs (length (^locs^)))
  (timeline-log! 'rules rule-counts)
  (timeline-log! 'outputs (length rewritten))

  (^children^ (append (^children^) rewritten))
  (^gened-rewrites^ #t)
  (void))

(define (num-nodes expr)
  (if (not (list? expr)) 1
      (add1 (apply + (map num-nodes (cdr expr))))))

(define (simplify!)
  (unless (^children^)
    (raise-user-error 'simplify! "No candidates generated. Run (gen-series!) or (gen-rewrites!)"))

  (when (flag-set? 'generate 'simplify)
    (timeline-event! 'simplify)

    (define locs-list
      (for/list ([child (^children^)] [n (in-naturals 1)])
        ;; We want to avoid simplifying if possible, so we only
        ;; simplify things produced by function calls in the rule
        ;; pattern. This means no simplification if the rule output as
        ;; a whole is not a function call pattern, and no simplifying
        ;; subexpressions that don't correspond to function call
        ;; patterns.
        (match (alt-event child)
          [(list 'taylor _ loc) (list loc)]
          [(list 'change cng)
           (match-define (change rule loc _) cng)
           (define pattern (rule-output rule))
           (define expr (location-get loc (alt-program child)))
           (cond
            [(not (list? pattern)) '()]
            [else
             (for/list ([pos (in-naturals 1)]
                        [arg-pattern (cdr pattern)] #:when (list? arg-pattern))
               (append (change-location cng) (list pos)))])]
          [_ (list '(2))])))

    (define to-simplify
      (for/list ([child (^children^)] [locs locs-list]
                 #:when true [loc locs])
        (location-get loc (alt-program child))))

    (define simplifications
      (simplify-batch to-simplify #:rules (*simplify-rules*) #:precompute true))

    (define simplify-hash
      (make-immutable-hash (map cons to-simplify simplifications)))

    (define simplified
      (for/list ([child (^children^)] [locs locs-list])
        (for/fold ([child child]) ([loc locs])
          (define child* (location-do loc (alt-program child) (λ (expr) (hash-ref simplify-hash expr))))
          (if (not (equal? (alt-program child) child*))
              (alt child* (list 'simplify loc) (list child))
              child))))

    (timeline-log! 'inputs (length locs-list))
    (timeline-log! 'outputs (length simplified))

    (^children^ simplified))
  (^simplified^ #t)
  (void))


;; Finish iteration
(define (finalize-iter!)
  (unless (^children^)
    (raise-user-error 'finalize-iter! "No candidates generated. Run (gen-series!) or (gen-rewrites!)"))

  (timeline-event! 'prune)
  (define new-alts (^children^))
  (define orig-fresh-alts (atab-not-done-alts (^table^)))
  (define orig-done-alts (set-subtract (atab-active-alts (^table^)) (atab-not-done-alts (^table^))))
  (^table^ (atab-add-altns (^table^) (^children^) (*output-repr*)))
  (define final-fresh-alts (atab-not-done-alts (^table^)))
  (define final-done-alts (set-subtract (atab-active-alts (^table^)) (atab-not-done-alts (^table^))))

  (timeline-log! 'inputs (+ (length new-alts) (length orig-fresh-alts) (length orig-done-alts)))
  (timeline-log! 'outputs (+ (length final-fresh-alts) (length final-done-alts)))

  (define data
    (hash 'new (list (length new-alts)
                     (length (set-intersect new-alts final-fresh-alts)))
          'fresh (list (length orig-fresh-alts)
                       (length (set-intersect orig-fresh-alts final-fresh-alts)))
          'done (list (- (length orig-done-alts) (if (^next-alt^) 1 0))
                      (- (length (set-intersect orig-done-alts final-done-alts))
                         (if (set-member? final-done-alts (^next-alt^)) 1 0)))
          'picked (list (if (^next-alt^) 1 0)
                        (if (and (^next-alt^) (set-member? final-done-alts (^next-alt^))) 1 0))))
  (timeline-log! 'kept data)

  (timeline-log! 'min-error (errors-score (atab-min-errors (^table^))))
  (rollback-iter!)
  (void))

(define (inject-candidate! prog)
  (^table^ (atab-add-altns (^table^) (list (make-alt prog)) (*output-repr*)))
  (void))

(define (finish-iter!)
  (when (not (^next-alt^))
    (debug #:from 'progress #:depth 3 "picking best candidate")
    (choose-best-alt!))
  (when (not (^locs^))
    (debug #:from 'progress #:depth 3 "localizing error")
    (localize!))
  (when (not (^gened-series^))
    (debug #:from 'progress #:depth 3 "generating series expansions")
    (gen-series!))
  (when (not (^gened-rewrites^))
    (debug #:from 'progress #:depth 3 "generating rewritten candidates")
    (gen-rewrites!))
  (when (not (^simplified^))
    (debug #:from 'progress #:depth 3 "simplifying candidates")
    (simplify!))
  (debug #:from 'progress #:depth 3 "adding candidates to table")
  (finalize-iter!)
  (void))

(define (rollback-iter!)
  (^children^ '())
  (^locs^ #f)
  (^next-alt^ #f)
  (^gened-rewrites^ #f)
  (^gened-series^ #f)
  (^simplified^ #f)
  (void))

(define (rollback-improve!)
  (rollback-iter!)
  (reset!)
  (^table^ #f)
  (void))

;; Run a complete iteration
(define (run-iter!)
  (when (^next-alt^)
    (raise-user-error 'run-iter! "An iteration is already in progress\n~a"
                      "Run (finish-iter!) to finish it, or (rollback-iter!) to abandon it.\n"))
  (debug #:from 'progress #:depth 3 "picking best candidate")
  (^children^ ; run on multiple alts
    (for/fold ([children '()]) ([altn (choose-mult-alts)] [i (in-naturals 1)]) 
      (define picking-func (λ (x) (for/first ([v x] #:when (alt-equal? v altn)) v)))
      (define-values (picked table*)
        (atab-pick-alt (^table^) #:picking-func picking-func #:only-fresh #t))
      (^next-alt^ picked)
      (^table^ table*)
      (debug #:from 'pick #:depth 4 (format "Picked [~a]: " i) picked)
      (debug #:from 'progress #:depth 3 "localizing error")
      (localize!)
      (debug #:from 'progress #:depth 3 "generating rewritten candidates")
      (gen-rewrites!)
      (debug #:from 'progress #:depth 3 "generating series expansions")
      (gen-series!)
      (debug #:from 'progress #:depth 3 "simplifying candidates")
      (simplify!)
      (debug #:from 'progress #:depth 3 "adding candidates to table")
      (let ([children (append children (^children^))])
        (rollback-iter!)
        children)))
  (finalize-iter!))

(define (run-improve prog iters
                     #:precondition [precondition #f]
                     #:precision [precision 'binary64]
                     #:specification [specification #f]
                     #:cost [cost-limit #f])
  (debug #:from 'progress #:depth 1 "[Phase 1 of 3] Setting up.")
  (setup-prog! prog
               #:specification specification
               #:precondition precondition
               #:precision precision
               #:cost cost-limit)
  (debug #:from 'progress #:depth 1 "[Phase 2 of 3] Improving.")
  (when (flag-set? 'setup 'simplify)
    (^children^ (atab-active-alts (^table^)))
    (simplify!)
    (finalize-iter!))
  (for ([iter (in-range iters)] #:break (atab-completed? (^table^)))
    (debug #:from 'progress #:depth 2 "iteration" (+ 1 iter) "/" iters)
    (run-iter!))
  (debug #:from 'progress #:depth 1 "[Phase 3 of 3] Extracting.")
  (extract!))

(define (final-simplify altn)
  (alt `(λ ,(program-variables (alt-program altn))
           ,(simplify-expr (program-body (alt-program altn))
                           #:rules (*fp-safe-simplify-rules*)))
        'final-simplify (list altn)))

(define (extract!)
  (define repr (*output-repr*))
  (define all-alts (atab-all-alts (^table^)))
  (*all-alts* all-alts)
  (define final-alts
    (cond
     [(and (flag-set? 'reduce 'regimes) (> (length all-alts) 1)
           (equal? (type-name (representation-type repr)) 'real)
           (not (null? (program-variables (alt-program (car all-alts))))))
      (compute-regimes all-alts repr (*sampler*))]
     [else
      (list (argmin score-alt all-alts))]))
  (timeline-event! 'simplify)
  (define cleaned-alts
    (cons (final-simplify (car final-alts))
          (parameterize ([*timeline-disabled* true])
            (map final-simplify (cdr final-alts)))))
  (timeline-event! 'end)
  cleaned-alts)
