#lang racket

(require json)

(define (check-pareto-files-exist! dirs)
  (for ([dir dirs])
    (unless (file-exists? (build-path dir "pareto.json"))
      (error 'check-pareto-files-exist!
             "Directory does not contain 'pareto.json: ~a\n"
             dir))))

(define (extract-json dir)
  (define path (build-path dir "pareto.json"))
  (define in (open-input-file path))
  (define h (read-json in))
  (close-input-port in)
  h)

(module+ main
 (command-line
  #:args dirs
  (check-pareto-files-exist! dirs)
  (define json-hashes (map extract-json dirs))
  (define pointss (map (curryr hash-ref 'points) json-hashes))
  (for ([dir dirs] [points pointss])
    (define y-max (argmax identity (map cadr points)))
    (define best-x
      (for/first ([point points] #:when (< (- y-max (cadr point)) 1))
        (car point)))
    (printf "~a : (~a, ~a)\n" dir best-x y-max))))