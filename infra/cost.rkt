#lang racket

(require json "../src/web/plot.rkt")

(define (extract-json dir)
  (define path (build-path dir "cost-time.json"))
  (define in (open-input-file path))
  (define h (read-json in))
  (close-input-port in)
  h)

(define (plot-points json-hash out-file)
  (define points (hash-ref json-hash 'points))
  (define points* (map (λ (l) (cons (car l) (cadr l))) points))
  (call-with-output-file (build-path out-file)
    #:exists 'replace
    (λ (out) (make-combined-cost-time-plot points* out))))

(module+ main
 (define out-file "./cost-time.pdf")
 (command-line
  #:once-each
  [("-o") out "Output directory"
   (set! out-file out)]
  #:args (dir)
  (unless (file-exists? (build-path dir "cost-time.json"))
    (error "Directory does not contain 'cost-time.json: ~a\n" dir))
  (define json-hash (extract-json dir))
  (plot-points json-hash out-file)))