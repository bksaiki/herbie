#lang racket

(require "alternative.rkt" "points.rkt")
(provide pareto-measure)

(define (alt-score alt context repr)
  (errors-score (errors (alt-program alt) context repr)))

(define (remove-ends l)
  (define len (length l))
  (cdr (take l (- len 1))))

;; Hypervolume measure of the pareto curve for cost vs. accuracy
(define (hypervolume-measure costs scores context repr)
  (define high-cost (last costs))
  (define low-cost (first costs))
  (define high-score (argmax identity scores))
  (define low-score (argmin identity scores))

  (define total-area (* (- high-score low-score) (- high-cost low-cost)))
  (define scores* (map (curryr - low-score) (remove-ends scores)))
  (define costs* (map (curryr - low-cost) (remove-ends costs)))
  (for/fold ([area 0] [cost0 0] #:result (/ area total-area))
            ([cost costs*] [score scores*])
    (values (+ area (* (- cost cost0) score)) cost)))

(define (paired-less? elem1 elem2)
  (let ([c1 (car elem1)] [c2 (car elem2)])
    (if (= c1 c2)
        (> (cdr elem1) (cdr elem2))
        (< c1 c2))))

(define (pareto-measure alts context repr)
  (if (> (length alts) 2)
      (let* ([scores (map (curryr alt-score context repr) alts)]
             [costs (map alt-cost alts)]
             [paired (map cons costs scores)]
             [paired* (sort paired paired-less?)])
        (hypervolume-measure (map car paired*) (map cdr paired*)
                             context repr))
      0))