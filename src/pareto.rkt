#lang racket

(require rackunit)
(require "alternative.rkt" "interface.rkt" "points.rkt")

(provide pareto-measure-alts pareto-measure-pnts compute-pareto-curve)

(define (alt-score alt context repr)
  (errors-score (errors (alt-program alt) context repr)))

(define (paired-less? elem1 elem2)
  (let ([c1 (car elem1)] [c2 (car elem2)])
    (if (= c1 c2)
        (> (cdr elem1) (cdr elem2))
        (< c1 c2))))

; Accepts a (sorted) list of points, returns the area under a curve above the line
; between (x-min, y-min) and (y-min, y-max).
(define (pareto-area pts)
  (match-define (list (cons xs ys) ...) pts)

  ; Find the smallest rectangle the contains all the points
  (define x-min (first xs))
  (define x-max (last xs))
  (define y-min (argmin identity ys))
  (define y-max (argmax identity ys))
  (define area/2 (* 1/2 (- x-max x-min) (- y-max y-min)))

  (cond
   [(zero? area/2) 0]
   [else
    ; Transform so (x-min, y-min) is the origin
    (define xs* (map (curryr - x-min) xs))
    (define ys* (map (curryr - y-min) ys))
    (define pts* (map cons xs* ys*))
    (define rsum ; triangular riemann sum
      (let loop ([p0 (car pts*)] [ps (cdr pts*)])
        (cond
        [(null? ps) 0]
        [else
          (define dx (- (caar ps) (car p0)))
          (define dy (- (cdar ps) (cdr p0)))
          (+ (* 1/2 dx dy) (* dx (cdr p0))
            (loop (car ps) (cdr ps)))])))
    (/ (- rsum area/2) area/2)]))

; Measure the pareto curve of a test
(define (pareto-measure-alts alts context repr)
  (cond
   [(< (length alts) 2) 0]
   [else
    (define bits (representation-total-bits repr))
    (define scores (map (λ (x) (- bits (alt-score x context repr))) alts))
    (define costs (map alt-cost alts))
    (define paired (map cons costs scores))
    (define paired* (sort paired paired-less?))
    (pareto-area paired*)]))

; Measure the area under the pareto curve (unsorted points)
(define (pareto-measure-pnts pnts)  
  (define pnts* (sort pnts paired-less?))
  (pareto-area pnts*))

(define *num-pareto-points* (make-parameter 100))

; This is really dumb
(define (brute-force-sums pts*)
  (define comb (apply cartesian-product pts*))
  (for/list ([pt comb])
    (cons (apply + (map car pt))
          (apply + (map cdr pt)))))

; Create a pareto curve across tests
(define (compute-pareto-curve pts)
  (cond
   [(null? pts) '()]
   [else
    (define pts* (map (curryr sort paired-less?) pts))
    (define sums (sort (brute-force-sums pts*) > #:key car))
    (define h (make-hash))
    (for ([sum sums])
      (hash-update! h (car sum) (λ (x) (cons (cdr sum) x)) (list (cdr sum))))
    (for/fold ([res '()] [last 0] #:result res)
              ([cost (sort (hash-keys h) <)])
      (let ([best (argmax identity (hash-ref h cost))])
        (cond [(> best last) (values (cons (cons cost best) res) best)]
              [else (values (cons (cons cost last) res) last)])))]))

(module+ test
  (define pts `(((1 . 2) (2 . 3) (3 . 4))
                ((3 . 3) (4 . 4) (5 . 5))
                ((2 . 1) (4 . 3) (6 . 5))))
  (define pareto (compute-pareto-curve pts))
  (displayln pareto))