#lang racket

(require math/bigfloat math/flonum)
(require "syntax/types.rkt")

(provide (struct-out representation) get-representation representation-name?
          *output-repr* *var-reprs* *needed-reprs* *reprs-with-rules*
          real->repr repr->real
          value? special-value?)

(module+ internals 
  (provide define-representation register-generator! register-representation!))

(define *reprs-with-rules* (make-parameter '()))
(define *needed-reprs* (make-parameter '()))

;; Structs

(struct representation
  (name type repr?
   bf->repr repr->bf ordinal->repr repr->ordinal
   total-bits special-values)
  #:methods gen:custom-write
  [(define (write-proc repr port mode)
     (fprintf port "#<representation ~a>" (representation-name repr)))])

(define representations (make-hash))

;; Repr / operator generation
;; Some plugins might define 'templated' reprs (e.g. fixed point with
;; m integer and n fractional bits). Since defining an infinite number of reprs
;; is impossible, Herbie stores a list of 'repr generators' to query if it comes
;; across a repr that is not known at the time. 

;; Generators take one argument, a repr name, and returns true if knows what the
;; repr is and has generated that repr and its operators, and false otherwise
(define repr-generators '())

(define (register-generator! proc)
  (-> (-> any/c boolean?))
  (set! repr-generators (cons proc repr-generators)))

(define (generate-repr repr-name)
  (for/or ([proc repr-generators])
    (proc repr-name)))

;; The set of reprs that Herbie comes across is collected here. This is the best place to guarantee 
;; that all the correct rules are generated but it'll collect more reprs than is necessary.
;; TODO: Find a better place to put this. Watch out for problems with multithreading / parameters. 
(define (get-representation name)
  (if (or (hash-has-key? representations name) ; check existing
          (generate-repr name)) ; ask plugins to try generating this repr
    (hash-ref representations name)
    (error 'get-representation "Unknown representation ~a" name))) ; else fail

(define (register-representation! name type repr? . args)
  (hash-set! representations name
            (apply representation name (get-type type) repr? args)))

(define-syntax-rule (define-representation (name type repr?) args ...)
  (register-representation! 'name 'type repr? args ...))

(define (representation-name? x)
  (with-handlers ([exn:fail? (const #f)])
    (get-representation x)
    true))

(define-representation (bool bool boolean?)
  identity
  identity
  (λ (x) (= x 0))
  (λ (x) (if x 1 0))
  1
  (const #f))

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))

(define-representation (binary64 real flonum?)
  bigfloat->flonum
  bf
  (shift 63 ordinal->flonum)
  (unshift 63 flonum->ordinal)
  64
  (disjoin nan? infinite?))

(define (single-flonum->bit-field x)
  (integer-bytes->integer (real->floating-point-bytes x 4) #f))

(define (single-flonum->ordinal x)
  (cond
    [(< x 0.0f0) (- (single-flonum->bit-field (- 0.0f0 x)))]
    [else (single-flonum->bit-field (abs x))]))

(define (bit-field->single-flonum x)
  (real->single-flonum (floating-point-bytes->real (integer->integer-bytes x 4 #f) #f)))

(define (ordinal->single-flonum x)
  (cond
    [(< x 0) (- (bit-field->single-flonum (- x)))]
    [else (bit-field->single-flonum x)]))

(define (single-flonum-step x n)
  (ordinal->single-flonum (+ (single-flonum->ordinal x) n)))

(define (bigfloat->single-flonum x)
  (define loprec (parameterize ([bf-precision 24]) (bf+ 0.bf x)))
  (define y (real->single-flonum (bigfloat->flonum loprec)))
  (define x2 (bf y))
  (match (bf-rounding-mode)
    ['nearest y]
    ['up   (if (bf< x2 x) (single-flonum-step y 1) y)]
    ['down (if (bf> x2 x) (single-flonum-step y -1) y)]
    ['zero (if (bf< x 0.bf)
               (if (bf< x2 x) (single-flonum-step y 1) y)
               (if (bf> x2 x) (single-flonum-step y -1) y))]))

(define-representation (binary32 real single-flonum?)
  bigfloat->single-flonum
  bf
  ordinal->single-flonum
  single-flonum->ordinal
  32
  (disjoin nan? infinite?))

;; repr <==> real

(define (real->repr x repr)
  (match x
   [(? (representation-repr? repr)) x] ; identity function if x is already a value in the repr
   [(? real?) ((representation-bf->repr repr) (bf x))]
   [_ (error 'real->repr "Unknown value ~a" x)]))

(define (repr->real x repr)
  (match x
    [(? boolean?) x]
    [_ (bigfloat->real ((representation-repr->bf repr) x))]))

;; Predicates

(define (value? x)
  (for/or ([(name repr) (in-hash representations)]) ((representation-repr? repr) x)))

(define (special-value? x repr)
  ((representation-special-values repr) x))

;; Global precision tracking
;; Leave this here else unit tests will fail
(define *output-repr* (make-parameter (get-representation 'binary64)))
(define *var-reprs* (make-parameter '()))