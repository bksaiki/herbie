#lang racket

(module+ test (require rackunit))

(provide is-fpsafe)

(define (is-fpsafe* in out)
  (match* (in out)
   [((list '* (list 'neg a) (list 'neg a)) (list '* x x))   ; neg cancellation
    (is-fpsafe* a x)]
   [((list '* (list 'fabs a) (list 'fabs a)) (list '* x x)) ; fabs cancellation
    (is-fpsafe* a x)]
   [((list 'neg (list '+ a b)) (list '+ x y))               ; neg distributive on +
    (and (is-fpsafe* (list 'neg a) x) (is-fpsafe* (list 'neg b) y))]
   [((list 'neg (list '* a b)) (list '* x y))               ; neg distributive on * 
    (or (and (is-fpsafe* (list 'neg a) x) (is-fpsafe* b y))
        (and (is-fpsafe* a x) (is-fpsafe* (list 'neg b) y)))]
   [((list 'neg (list '/ a b)) (list '/ x y))               ; neg distributive on /
    (or (and (is-fpsafe* (list 'neg a) x) (is-fpsafe* b y))
        (and (is-fpsafe* a x) (is-fpsafe* (list 'neg b) y)))]
   [((list 'fabs (list '* a a)) (list '* x x))              ; fabs squaring
    (is-fpsafe* a x)]
   [((list 'fabs (list '* a b)) (list '* x y))              ; fabs distributive on * 
    (and (is-fpsafe* (list 'fabs a) x) (is-fpsafe* (list 'fabs b) y))]
   [((list 'fabs (list '/ a b)) (list '/ x y))              ; fabs distributive on /
    (and (is-fpsafe* (list 'fabs a) x) (is-fpsafe* (list 'fabs b) y))]
   [((list 'fabs (list '- a b)) (list 'fabs (list '- x y))) ; fabs, - commutativity
    (or (and (is-fpsafe* a x) (is-fpsafe* b y))
        (and (is-fpsafe* a y) (is-fpsafe* b x)))]
   [((list 'fabs (list 'neg a)) (list 'fabs x))             ; fabs, neg cancellation
    (is-fpsafe* a x)]
   [((list '+ a b) (list '+ x y))                           ; + commutativity; +, +
    (or (and (is-fpsafe* a x) (is-fpsafe* b y))
        (and (is-fpsafe* a y) (is-fpsafe* b x)))]
   [((list '- a b) (list '- x y))                           ; -, -
    (and (is-fpsafe* a x) (is-fpsafe* b y))]
   [((list '* a b) (list '* x y))                           ; * commutativity; *, *
    (or (and (is-fpsafe* a x) (is-fpsafe* b y))
        (and (is-fpsafe* a y) (is-fpsafe* b x)))]
   [((list '/ a b) (list '/ x y))                           ; /, /
    (and (is-fpsafe* a x) (is-fpsafe* b y))]
   [((list 'neg a) (list 'neg x))                           ; neg, neg
    (is-fpsafe* a x)]
   [((list 'fabs a) (list 'fabs x))                         ; fabs, fabs
    (is-fpsafe* a x)]
   [((list '- a b) (list '+ x y))                           ; (- a b) => (+ a (neg b))
    (and (is-fpsafe* a x) (is-fpsafe* (list 'neg b) y))]
   [((list 'neg (list 'neg a)) _)                           ; (neg (neg a)) => a
    (is-fpsafe* a out)]
   [((list 'fabs (list fabs a)) _)                          ; (fabs (fabs a)) => a
    (is-fpsafe* a out)]
   [((list '+ 0 a) _)                                       ; (+ 0 a) => a
    (is-fpsafe* a out)]
   [((list '+ a 0) _)                                       ; (+ a 0) => a
    (is-fpsafe* a out)]
   [((list '- 0 a) _)                                       ; (- 0 a) => (neg a)
    (is-fpsafe* (list 'neg a) out)]
   [((list '- a 0) _)                                       ; (- a 0) => a
    (is-fpsafe* a out)]
   [((list '* 1 a) _)                                       ; (* 1 a) => a
    (is-fpsafe* a out)]
   [((list '* a 1) _)                                       ; (* a 1) => a
    (is-fpsafe* a out)]
   [((list '/ a 1) _)                                       ; (/ a 1) => a
    (is-fpsafe* a out)]
   [((list '* -1 a) _)                                      ; (* -1 a) => a
    (is-fpsafe* (list 'neg a) out)]
   [((list '* a -1) _)                                      ; (* a -1) => a
    (is-fpsafe* (list 'neg a) out)]
   [((list '- a a) _)                                       ; (- a a) => 0
    (is-fpsafe* 0 out)]
   [((list '/ a a) _)                                       ; (/ a a) => 1
    (is-fpsafe* 1 out)]
   [((list '/ 0 a) _)                                       ; (/ 0 a) => 0
    (is-fpsafe* 0 out)]
   [((list '* 0 a) _)                                       ; (* 0 a) => 0
    (is-fpsafe* 0 out)]
   [((list '* a 0) _)                                       ; (* a 0) => 0
    (is-fpsafe* 0 out)]
   [(_ _)  (equal? in out)]))
  
(define (is-fpsafe in out)
  (or (is-fpsafe* in out) (is-fpsafe* out in)))

(module+ test
  ;; commutativity
  (check-true (is-fpsafe '(+ a b) '(+ b a)))
  (check-true (is-fpsafe '(* a b) '(* b a)))
  
  ;; neg distributivity (+, *, /)
  (check-true (is-fpsafe '(neg (* a b)) '(* (neg a) b)))
  (check-true (is-fpsafe '(neg (* a b)) '(* a (neg b))))
  (check-true (is-fpsafe '(* (neg a) b) '(neg (* a b))))
  (check-true (is-fpsafe '(* a (neg b)) '(neg (* a b))))
  (check-true (is-fpsafe '(neg (/ a b)) '(/ (neg a) b)))
  (check-true (is-fpsafe '(neg (/ a b)) '(/ a (neg b))))
  (check-true (is-fpsafe '(/ (neg a) b) '(neg (/ a b))))
  (check-true (is-fpsafe '(/ a (neg b)) '(neg (/ a b))))
  (check-true (is-fpsafe '(neg (+ a b)) '(+ (neg a) (neg b))))
  (check-true (is-fpsafe '(+ (neg a) (neg b)) '(neg (+ a b))))

  ;; definition of subtraction
  (check-true (is-fpsafe '(- a b) '(+ a (neg b))))
  (check-true (is-fpsafe '(+ a (neg b)) '(- a b)))
  (check-true (is-fpsafe '(- a (* (neg b) c)) '(+ a (* b c))))
  (check-true (is-fpsafe '(- a (* b c)) '(+ a (* (neg b) c))))

  ;; reduction to constants
  (check-true (is-fpsafe '(- a a) 0))
  (check-true (is-fpsafe '(/ a a) 1))
  (check-true (is-fpsafe '(/ 0 a) 0))
  (check-true (is-fpsafe '(* 0 a) 0))
  (check-true (is-fpsafe '(* a 0) 0))

  ;; identity
  (check-true (is-fpsafe '(+ 0 a) 'a))
  (check-true (is-fpsafe '(+ a 0) 'a))
  (check-true (is-fpsafe '(- 0 a) '(neg a)))
  (check-true (is-fpsafe '(* 1 a) 'a))
  (check-true (is-fpsafe '(* a 1) 'a))
  (check-true (is-fpsafe '(/ a 1) 'a))
  (check-true (is-fpsafe '(* -1 a) '(neg a)))
  (check-true (is-fpsafe '(* a -1) '(neg a)))

  ;; square
  (check-true (is-fpsafe '(* (neg a) (neg a)) '(* a a)))
  (check-true (is-fpsafe '(* (fabs a) (fabs a)) '(* a a)))

  ; fabs
  (check-true (is-fpsafe '(fabs (fabs a)) 'a))
  (check-true (is-fpsafe '(fabs (- a b)) '(fabs (- b a))))
  (check-true (is-fpsafe '(fabs (neg a)) '(fabs a)))
  (check-true (is-fpsafe '(fabs (* a a)) '(* a a)))
  (check-true (is-fpsafe '(fabs (* a b)) '(* (fabs a) (fabs b))))
  (check-true (is-fpsafe '(fabs (/ a b)) '(/ (fabs a) (fabs b))))

  ;; other rules
  (check-false (is-fpsafe '(+ (+ b c)) '(+ (+ a b) c)))
  (check-false (is-fpsafe '(+ x x) '(* 2 x)))
  (check-false (is-fpsafe '(* a (+ b c)) '(+ (* a b) (* a c))))
  (check-false (is-fpsafe '(- (* a a) 1) '(* (+ a 1) (- a 1))))
  (check-false (is-fpsafe '(/ 1 (/ 1 a)) 'a))
  (check-false (is-fpsafe '(* a (/ 1 b)) '(/ a b)))
  (check-false (is-fpsafe '(/ a b) '(/ (neg a) (neg b))))
)
