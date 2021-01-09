#lang racket

(require json "../src/web/plot.rkt")

(define (check-pareto-files-exist! dirs)
  (for ([dir dirs])
    (unless (file-exists? (build-path dir "pareto.json"))
      (error "Directory does not contain 'pareto.json: ~a\n" dir))))

(define (extract-json dir)
  (define path (build-path dir "pareto.json"))
  (define in (open-input-file path))
  (define h (read-json in))
  (close-input-port in)
  h)

(define (plot-points json-hashes out-file)
  (define names (map (curryr hash-ref 'branch) json-hashes))
  (define ymaxs (map (curryr hash-ref 'y-max) json-hashes))
  (define ptss (map (curryr hash-ref 'points) json-hashes))
  (define ptss* (map (curry map (λ (l) (cons (car l) (cadr l)))) ptss))

  (unless (apply = ymaxs)
    (error 'plot-points "Maximum y values do not match"))

  (define x-max
    (for/fold ([x-max 0]) ([pts ptss*])
      (apply max x-max (map car pts))))
  (define y-max (car ymaxs))
  (call-with-output-file (build-path out-file)
    #:exists 'replace
    (λ (out) (make-combined-cost-accuracy-plot names ptss* x-max y-max out))))

(module+ main
 (define out-file "./")
 (command-line
  #:once-each
  [("-o") out "Output directory"
   (set! out-file out)]
  #:args dirs
  (check-pareto-files-exist! dirs)
  (define json-hashes (map extract-json dirs))
  (plot-points json-hashes out-file)))