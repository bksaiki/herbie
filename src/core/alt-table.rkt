#lang racket

(require racket/hash)
(require "../common.rkt" "../alternative.rkt" "../points.rkt"
         "../timeline.rkt" "../programs.rkt")

(provide
 (contract-out
  (make-alt-table (pcontext? alt? any/c . -> . alt-table?))
  (atab-active-alts (alt-table? . -> . (listof alt?)))
  (atab-all-alts (alt-table? . -> . (listof alt?)))
  (atab-not-done-alts (alt-table? . -> . (listof alt?)))
  (atab-add-altns (alt-table? (listof alt?) any/c . -> . alt-table?))
  (atab-pick-alt (alt-table? #:picking-func ((listof alt?) . -> . alt?)
                             #:only-fresh boolean?
                             . -> . (values alt? alt-table?)))
  (atab-peek-alt (alt-table? #:picking-func ((listof alt?) . -> . alt?)
                             #:only-fresh boolean?
                             . -> . (values alt? alt-table?)))
  (atab-completed? (alt-table? . -> . boolean?))
  (atab-context (alt-table? . -> . pcontext?))
  (atab-min-errors (alt-table? . -> . (listof real?)))
  (split-atab (alt-table? (non-empty-listof any/c) . -> . (listof alt-table?)))))

;; Public API

(struct alt-table (point->alts alt->points alt->done? alt->cost context all) #:prefab)
(struct cost-rec (berr altns) #:prefab)

(define atab-context alt-table-context)

(define in-atab-pcontext (compose in-pcontext atab-context))

(define (make-alt-table context initial-alt repr)
  (define cost (alt-cost initial-alt))
  (alt-table (make-immutable-hash
               (for/list ([(pt ex) (in-pcontext context)]
                          [err (errors (alt-program initial-alt) context repr)])
                 (cons pt (hash cost (cost-rec err (list initial-alt))))))
             (hash initial-alt (for/list ([(pt ex) (in-pcontext context)]) pt))
             (hash initial-alt #f)
             (hash initial-alt cost)
             context
             (mutable-set initial-alt)))

(define (atab-pick-alt atab #:picking-func [pick car]
           #:only-fresh [only-fresh? #t])
  (let* ([picked (atab-peek-alt atab #:picking-func pick #:only-fresh only-fresh?)]
         [atab* (struct-copy alt-table atab
                             [alt->done? (hash-set (alt-table-alt->done? atab)
                                                   picked #t)])])
    (values picked atab*)))

(define (atab-peek-alt atab #:picking-func [pick car] #:only-fresh [only-fresh? #f])
  (pick (if only-fresh?
      (atab-not-done-alts atab)
      (atab-active-alts atab))))

(define (atab-active-alts atab)
  (hash-keys (alt-table-alt->points atab)))

(define (atab-all-alts atab)
  (set->list (alt-table-all atab)))

(define (atab-completed? atab)
  (andmap identity (hash-values (alt-table-alt->done? atab))))

;; Split the alt table into several alt tables, each of which corresponds to a pred
;; in 'preds', and only contains points which satisfy that pred.
(define (split-atab atab preds)
  (for/list ([pred preds])
    (let* ([point->alts (make-immutable-hash (for/list ([(pt ex) (in-atab-pcontext atab)]
              #:when (pred pt))
                 (cons pt (hash-ref (alt-table-point->alts atab) pt))))]
     [alt->points (make-immutable-hash (filter (compose (negate null?) cdr)
                 (for/list ([(alt points)
                 (in-hash (alt-table-alt->points atab))])
                   (cons alt (filter pred points)))))]
     [alt->done? (make-immutable-hash (for/list ([alt (in-hash-keys alt->points)])
                (cons alt (hash-ref (alt-table-alt->done? atab) alt))))]
     [context (call-with-values
      (λ () (for/lists (pts exs)
          ([(pt ex) (in-atab-pcontext atab)]
           #:when (pred pt))
        (values pt ex)))
          mk-pcontext)])
      (minimize-alts (alt-table point->alts alt->points alt->done?
                                context (alt-table-all atab))))))

;; Helper Functions

(define (hash-remove* hash keys)
  (for/fold ([hash hash]) ([key keys])
    (hash-remove hash key)))

;; Implementation

(struct point-rec (berr cost altns) #:prefab)

(define (best-and-tied-at-points atab altn cost errs)
  (define point->alt (alt-table-point->alts atab))
  (reap [best! tied! tied-errs!]
    (for ([(pnt ex) (in-pcontext (alt-table-context atab))] [err errs])
      (define cost-hash (hash-ref point->alt pnt))
      (define rec (hash-ref cost-hash cost #f))
      (if rec
          (let ([table-err (cost-rec-berr rec)])
            (cond
             [(< err table-err)
              (best! pnt)]
             [(= err table-err)
              (tied! pnt)
              (tied-errs! err)]
             [else (void)]))
          (best! pnt)))))

(define (remove-chnged-pnts point->alts alt->points alt->cost chnged-pnts cost)
  (define chnged-entries (map (curry hash-ref point->alts) chnged-pnts))
  (define chnged-altns (mutable-set))
  (for* ([cost-hash chnged-entries]
         [rec (hash-values cost-hash)]
         [altn (cost-rec-altns rec)])
    (when (equal? (hash-ref alt->cost altn) cost)
      (set-add! chnged-altns altn)))
  (hash-union
   alt->points
   (for/hash ([altn (in-set chnged-altns)])
     (values altn (remove* chnged-pnts (hash-ref alt->points altn))))
   #:combine (λ (a b) b)))

(define (override-at-pnts points->alts pnts altn cost errs)
  (define pnt->errs
    (for/hash ([(pnt ex) (in-pcontext (*pcontext*))] [err errs])
                        (values pnt err)))
  (hash-union
   points->alts
   (for/hash ([pnt pnts])
     (values pnt (hash cost (cost-rec (hash-ref pnt->errs pnt) (list altn)))))
   #:combine (λ (a b) (hash-union a b #:combine (λ (x y) y)))))

(define (append-at-pnts points->alts pnts altn cost)
  (hash-union
   points->alts
   (for/hash ([pnt pnts])
     (define cost-hash (hash-ref points->alts pnt))
     (match-define (cost-rec berr altns) (hash-ref cost-hash cost))
     (values pnt (hash-set cost-hash cost (cost-rec berr (cons altn altns)))))
   #:combine (λ (a b) b)))

(define (minimize-alts atab)
  (define (get-essential pnts->alts)
    (define essential (mutable-set))
    (for* ([cost-hash (hash-values pnts->alts)]
           [rec (hash-values cost-hash)])
      (let ([altns (cost-rec-altns rec)])
        (cond
         [(> (length altns) 1) (void)]
         [(= (length altns) 1) (set-add! essential (car altns))]
         [else (error "This point has no alts which are best at it!" rec)])))
    (set->list essential))

  (define (get-tied-alts essential-alts alts->pnts pnts->alts)
    (remove* essential-alts (hash-keys alts->pnts)))

  (define (worst atab altns)
    (let* ([alts->pnts (curry hash-ref (alt-table-alt->points atab))]
           [alts->done? (curry hash-ref (alt-table-alt->done? atab))]
           [alt->cost (curry hash-ref (alt-table-alt->cost atab))]
      ; There must always be a not-done tied alt,
      ; since before adding any alts there weren't any tied alts
           [undone-altns (filter (compose not alts->done?) altns)])
      (argmax
        alt->cost
        (argmins (compose length alts->pnts) (if (null? undone-altns) altns undone-altns)))))

  (let loop ([cur-atab atab])
    (let* ([alts->pnts (alt-table-alt->points cur-atab)]
           [pnts->alts (alt-table-point->alts cur-atab)]
           [essential-alts (get-essential pnts->alts)]
           [tied-alts (get-tied-alts essential-alts alts->pnts pnts->alts)])
      (if (null? tied-alts)
          cur-atab
          (loop (rm-alts cur-atab (worst cur-atab tied-alts)))))))

(define (rm-alts atab . altns)
  (match-define (alt-table point->alts alt->points alt->done? alt->cost _ _) atab)
  (define rel-points
    (remove-duplicates
     (apply append (map (curry hash-ref (alt-table-alt->points atab)) altns))))

  (define pnts->alts*
    (hash-union
      point->alts
      (for/hash ([pnt rel-points])
        (define cost-hash
          (for/hash ([(cost rec) (hash-ref point->alts pnt)])
            (values cost (struct-copy cost-rec rec
                                      [altns (remove* altns (cost-rec-altns rec))]))))
        (values pnt cost-hash))
      #:combine (λ (a b) b)))

  (struct-copy alt-table atab
               [point->alts pnts->alts*]
               [alt->points (hash-remove* alt->points altns)]
               [alt->done? (hash-remove* alt->done? altns)]
               [alt->cost (hash-remove* alt->cost altns)]))

(define (atab-add-altns atab altns repr)
  (define altns* (remove-duplicates altns alt-equal?))
  (define progs (map alt-program altns*))
  (define errss (apply vector-map list (batch-errors progs (alt-table-context atab) repr)))
  (for/fold ([atab atab]) ([altn (in-list altns*)] [errs (in-vector errss)])
    (atab-add-altn atab altn errs repr)))

(define (atab-add-altn atab altn errs repr)
  (define cost (alt-cost altn))
  (match-define (alt-table point->alts alt->points alt->done? alt->cost _ all-alts) atab)
  (define-values (best-pnts tied-pnts tied-errs) (best-and-tied-at-points atab altn cost errs))
  (cond
   [(and (null? best-pnts)
         (or
           (ormap (curry alt-equal? altn) (hash-keys alt->points))
           (for/and ([pt tied-pnts] [err tied-errs])
             (let* ([cost-table (hash-ref point->alts pt)]
                    [maxcost (argmax identity (hash-keys cost-table))]
                    [maxerr (cost-rec-berr (hash-ref cost-table maxcost))])
               (and (>= cost maxcost) (>= err maxerr))))))
    atab]
   [else
    (define alts->pnts*1 (remove-chnged-pnts point->alts alt->points alt->cost best-pnts cost))
    (define alts->pnts*2 (hash-set alts->pnts*1 altn (append best-pnts tied-pnts)))
    (define pnts->alts*1 (override-at-pnts point->alts best-pnts altn cost errs))
    (define pnts->alts*2 (append-at-pnts pnts->alts*1 tied-pnts altn cost))
    (define alts->done?* (hash-set alt->done? altn #f))
    (define alt->cost* (hash-set alt->cost altn cost))
    (when (not (for/or ([x (in-mutable-set all-alts)]) (alt-equal? altn x)))
      (set-add! all-alts altn)) ; only add if unique
    (minimize-alts (alt-table pnts->alts*2 alts->pnts*2 alts->done?*
                              alt->cost* (alt-table-context atab) all-alts))]))

(define (atab-not-done-alts atab)
  (filter (negate (curry hash-ref (alt-table-alt->done? atab)))
    (hash-keys (alt-table-alt->points atab))))

(define (atab-min-errors atab)
  (define pnt->alts (alt-table-point->alts atab))
  (for/list ([(pt ex) (in-pcontext (alt-table-context atab))])
    (for/fold ([best #f]) ([rec (hash-values (hash-ref pnt->alts pt))])
      (let ([err (cost-rec-berr rec)])
        (cond [(not best) err]
              [(< err best) err]
              [(>= err best) best])))))

;; The completeness invariant states that at any time, for every point there exists some
;; alt that is best at it.
(define (check-completeness-invariant atab #:message [message ""])
  (if (for/and ([(pt ch) (in-hash (alt-table-point->alts atab))])
        (for/or ([(c rec) (in-hash ch)])
          (not (null? (cost-rec-altns rec)))))
      atab
      (error (string-append "Completeness invariant violated. " message))))

(define (pnt-maps-to-alt? pt altn pnt->alts)
  (define cost-hash (hash-ref pnt->alts pt))
  (for ([rec (hash-values cost-hash)])
    (member altn (cost-rec-altns rec))))

(define (alt-maps-to-pnt? altn pt alt->pnts)
  (member pt (hash-ref alt->pnts altn)))

;; The reflexive invariant is this: a) For every alternative, for every point it maps to,
;; those points also map back to the alternative. b) For every point, for every alternative
;; it maps to, those alternatives also map back to the point.
(define (check-reflexive-invariant atab #:message [message ""])
  (define pnt->alts (alt-table-point->alts atab))
  (define alt->pnts (alt-table-alt->points atab))
  (if (and 
        (for/and ([(altn pnts) (in-hash alt->pnts)])
          (andmap (curryr pnt-maps-to-alt? altn pnt->alts) pnts))
        (for/and ([(pt ch) (in-hash pnt->alts)])
          (for/and ([(c rec) (in-hash ch)])
            (andmap (curryr alt-maps-to-pnt? pt alt->pnts) (cost-rec-altns rec)))))
      atab
      (error (string-append "Reflexive invariant violated. " message))))

;; The minimality invariant states that every alt must be untied and best on at least one point.
(define (check-minimality-invariant atab #:message [message ""])
  (hash-for-each (alt-table-alt->points atab)
                 (λ (k v)
                    (let ([cnt (for/list ([pt v])
                                 (let ([cost (alt-cost k)]
                                       [cost-hash (hash-ref (alt-table-point->alts atab) pt)])
                                   (length (cost-rec-altns (hash-ref cost-hash cost)))))])
                      (when (not (= (apply min cnt) 1))
                        (error (string-append "Minimality invariant violated. " message)))))))


(define (assert-points-orphaned alts->pnts opnts all-pnts #:message [msg ""])
  (hash-for-each alts->pnts
     (λ (altn pnts)
       (when (ormap (curryr member pnts) opnts)
         (error (string-append "Assert Failed: The given points were not orphaned. " msg)))))
  (let ([hopefully-unorphaned-points (remove* opnts all-pnts)]
  [actually-unorphaned-points (remove-duplicates (apply append (hash-values alts->pnts)))])
    (when (ormap (negate (curryr member actually-unorphaned-points)) hopefully-unorphaned-points)
      (error (string-append "Assert Failed: Points other than the given points were orphaned. " msg)))))
