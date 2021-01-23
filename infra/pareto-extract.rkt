#lang racket

(require json racket/path)
(require "../src/datafile.rkt" "../src/interface.rkt" "../src/pareto.rkt" "../src/plugin.rkt"
         "../src/programs.rkt" "../src/syntax/sugar.rkt")

(define (extract-json dir)
  (define seed-dirs
    (filter directory-exists? 
      (map (curry build-path dir) (directory-list (build-path dir)))))
  (define hs (make-hash))
  (for ([seed-dir seed-dirs])
    (define info (read-datafile (build-path seed-dir "results.json")))
    (define tests (report-info-tests info))
    (define hs* (make-hash))
    (for ([test tests] #:when (table-row-result-est test))
      (define name (table-row-name test))
      (define repr (get-representation (table-row-precision test)))
      (define vars (table-row-vars test))
      (define err (table-row-result-est test))
      (define var-reprs (map cons vars (build-list (length vars) (const repr))))
      (define prog* `(λ ,vars ,(desugar-program (table-row-output test) repr var-reprs)))
      (define cost (program-cost prog*))
      
      (define h (make-hash))
      (hash-set! h 'bits (representation-total-bits repr))
      (hash-set! h 'points (list (cons cost (- (representation-total-bits repr) err))))
      (hash-set! hs* name h))
    
    (for ([(name h) (in-hash hs*)])
      (hash-update! hs name
        (λ (x) (hash-update! x 'points (curry append (hash-ref h 'points))) x) 
        h)))
  hs)

(define (pareto->json points ymax out)
  (define h (make-hasheq))
  (define pareto-points* (map (λ (p) (list (car p) (cdr p))) points))
  (hash-set! h 'points pareto-points*)
  (hash-set! h 'branch "rewriting")
  (hash-set! h 'y-max ymax)
  (write-json h out))

(module+ main
 (define out-file "./pareto.json")
 (command-line
  #:once-each
  [("-o") out "Output directory"
   (set! out-file out)]
  #:args (dir)
  (load-herbie-plugins)
  (define h (extract-json dir))
  (define ptss
    (for/list ([(k v) (in-hash h)])
      (hash-ref v 'points)))
  (define ymax (apply + (map (curryr hash-ref 'bits) (hash-values h))))
  (define pareto (compute-pareto-curve ptss))
  (call-with-output-file (build-path out-file)
    #:exists 'replace
    (λ (out) (pareto->json pareto ymax out)))))