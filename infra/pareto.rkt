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

  (unless (apply = ymaxs)
    (displayln "WARN: Maximum y values do not match"))

  (define ymaxmax (apply max ymaxs))
  (define dys (map (curry - ymaxmax) ymaxs))

  (define ptss* 
    (for/list ([pts ptss] [dy dys])
      (for/list ([pt pts])
        (cons (car pt) (+ (cadr pt) dy)))))

  (define x-max
    (for/fold ([x-max 0]) ([pts ptss*])
      (apply max x-max (map car pts))))
  (define y-max (car ymaxs))
  (call-with-output-file (build-path out-file)
    #:exists 'replace
    (λ (out) (make-combined-cost-accuracy-plot names ptss* x-max y-max out))))

(define (alt-plot json-hashes dirs y-max out-file)
  (for ([dir dirs] [json-hash json-hashes])
    (define points (hash-ref json-hash 'points))
    (define start (hash-ref json-hash 'start))
    (define start* (cons (car start) (cadr start)))
    (define points* (map (λ (l) (cons (car l) (cadr l))) points))
    (call-with-output-file (build-path out-file)
      #:exists 'replace
     (λ (out) (make-single-cost-accuracy-plot points* start* y-max out)))))


(module+ main
 (define out-file "pareto.pdf")
 (define mode 'standard)
 (define alt-ymax 64)
 (command-line
  #:once-each
  [("-o") out "Output directory"
   (set! out-file out)]
  [("--alt") "Alternative mode"
   (set! mode 'alternative)]
  [("--ymax") num "Y-max in alternative mode"
    (set! alt-ymax (string->number num))]
  #:args dirs
  (check-pareto-files-exist! dirs)
  (define json-hashes (map extract-json dirs))
  (match mode
   ['alternative (alt-plot json-hashes dirs alt-ymax out-file)]
   [_ (plot-points json-hashes out-file)])))