#lang racket

(provide (struct-out pareto-point) pareto-map pareto-union combine-pareto)

(struct pareto-point (cost error data) #:prefab)

(define (pareto-compare pt1 pt2)
  (match-define (pareto-point cost1 err1 data1) pt1)
  (match-define (pareto-point cost2 err2 data2) pt2)
  (cond
   [(and (= cost1 cost2)  (= err1 err2))  '=]
   [(and (<= cost1 cost2) (<= err1 err2)) '<]
   [(and (>= cost1 cost2) (>= err1 err2)) '>]
   [else '<>]))

(define (pareto-map f curve)
  (for/list ([ppt (in-list curve)])
    (struct-copy pareto-point ppt [data (f (pareto-point-data ppt))])))

(module+ test
  (require rackunit)

  (define (make-pareto pts)
    (sort
     (for/list ([pt (in-list pts)])
       (match-define (list cost err altns ...) pt)
       (pareto-point cost err altns))
     < #:key pareto-point-error))

  (define (from-pareto pts)
    (sort 
     (for/list ([ppt (in-list pts)])
       (match-define (pareto-point cost err altns) ppt)
       (list* cost err altns))
     < #:key first))
  
  (define (pareto-add curve d c e)
    (pareto-union (list (pareto-point c e (list d))) curve))

  (check-equal? (from-pareto (make-pareto '((1 5 a) (2 3 b) (5 1 a b))))
                '((1 5 a) (2 3 b) (5 1 a b)))
  (check-equal? (from-pareto (pareto-add (make-pareto '()) 'a 1 5))
                '((1 5 a)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 5 a) (5 1 b))) 'c 3 3))
                '((1 5 a) (3 3 c) (5 1 b)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 5 a) (3 3 b))) 'c 5 1))
                '((1 5 a) (3 3 b) (5 1 c)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((3 3 b) (5 1 c))) 'a 1 5))
                '((1 5 a) (3 3 b) (5 1 c)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 5 a) (3 3 b) (5 1 c))) 'd 1 5))
                '((1 5 d a) (3 3 b) (5 1 c)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 5 a) (3 3 b) (5 1 c))) 'd 3 3))
                '((1 5 a) (3 3 d b) (5 1 c)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 5 a) (3 3 b) (5 1 c))) 'd 2 2))
                '((1 5 a) (2 2 d) (5 1 c)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 1 a))) 'b 1 3))
                '((1 1 a))))


(define (pareto-union curve1 curve2)
  (let loop ([curve1 curve1] [curve2 curve2])
    ; The curve is sorted so that highest accuracy is first
    (match* (curve1 curve2)
      [('() _) curve2]
      [(_ '()) curve1]
      [((cons ppt1 rest1) (cons ppt2 rest2))
       (match (pareto-compare ppt1 ppt2)
         ['<
          (loop curve1 rest2)]
         ['>
          (loop rest1 curve2)]
         ['=
          (define joint
            (struct-copy pareto-point ppt1
                         [data (append (pareto-point-data ppt1) (pareto-point-data ppt2))]))
          (cons joint (loop rest1 rest2))]
         ['<>
          (if (< (pareto-point-error ppt1) (pareto-point-error ppt2))
              (cons ppt1 (loop rest1 curve2))
              (cons ppt2 (loop curve1 rest2)))])])))

;; Takes a pareto frontier (decreasing, x >= 0) and returns the points
;; that form a convex frontier (montonically decreasing)
(define (convex-pareto pts)
  (let loop ([pts* '()] [pts pts])
    (match pts
     [(list p0 p1 p2 pns ...)
      (define m01 (/ (- (second p1) (second p0)) (- (first p1) (first p0))))
      (define m12 (/ (- (second p2) (second p1)) (- (first p2) (first p1))))
      ; if { p0, p1, p2 } are not convex:
      ;   discard p1
      ;   try backtracking one point (if not continue)
      ; else move forward one point
      (if (< m12 m01)
          (if (null? pts*)
              (loop pts* (append (list p0 p2) pns))
              (loop (cdr pts*) (append (list (car pts*) p0 p2) pns)))
          (loop (cons (car pts) pts*) (cdr pts)))]
     [_ (append (reverse pts*) pts)])))

; Take the first set of points {(x, y)}
;   If x_i = x_j, take the point with higher y
;   As x increases, y should decrease;
;     remove points that break this property
; Add the remaining points to the next set of points (cartesian product)
;   to form a new set of (n * m) points
;   Repeat the process above ...
(define (sum-pareto-pnts pts)
  (let loop ([pts pts] [h (make-hash `((0 . 0)))]) ; keep a hash of costs and partial sums
    (cond
     [(null? pts) h]
     [(null? (car pts)) (loop (cdr pts) h)]
     [else
      (define h* (make-hash))
      (for* ([(x y) (in-hash h)] [pt (car pts)])  ; make a new hash: h + pts, dedup by taking max
        (hash-update! h* (+ x (car pt))
                      (λ (x) (min x (+ y (cdr pt))))
                      (+ y (cdr pt))))
      (for/fold ([best +inf.0]) ([x (sort (hash-keys h*) <)]) 
        (let ([y (hash-ref h* x)])  ; as x increases, y must decrease; remove increased points
          (cond
           [(< y best) y]
           [else
            (hash-remove! h* x)
            best])))
      (loop (cdr pts) h*)])))

(define (paired-less? elem1 elem2)
  (let ([c1 (car elem1)] [c2 (car elem2)])
    (if (= c1 c2)
        (> (cdr elem1) (cdr elem2))
        (< c1 c2))))

;; Creates a synthetic frontier from multiple frontiers
;; as described in the ARITH '21 paper.
(define (combine-pareto frontiers #:convex? [convex? #f])
  (define pts
    (for/list ([frontier frontiers])
      (for/list ([pt frontier])
        (cons (first pt) (second pt)))))
  (define pts* (map (curryr sort paired-less?) pts))
  (define coords (hash->list (sum-pareto-pnts pts*)))
  (define sorted (sort coords < #:key car))
  (map (λ (x) (list (car x) (cdr x))) (if convex? (convex-pareto sorted) sorted)))
