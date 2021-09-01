#lang racket

(require pkg/lib json)

(provide (struct-out rule-manifest)
         (struct-out ruler-rule)
         read-cache clear-cache
         make-cache-directory)

(define cache-dir (build-path (pkg-directory "ruler-herbie") "cache"))

(struct ruler-rule (lhs rhs bi?))
(struct rule-manifest (name iters varc fuzzc final? rules))

(define (rule-manifest=? x y)
  (and (equal? (rule-manifest-iters x) (rule-manifest-iters y))
       (equal? (rule-manifest-varc x) (rule-manifest-varc y))
       (equal? (rule-manifest-fuzzc x) (rule-manifest-fuzzc y))
       (equal? (rule-manifest-final? x) (rule-manifest-final? y))))

(define (json->rules eqs)
  (for/list ([r (in-list eqs)])
    (let ([get (λ (k) (hash-ref r k))])
      (ruler-rule (get 'lhs)
                  (get 'rhs)
                  (get 'bidirectional)))))

(define (json->manifest file)
  (define json (call-with-input-file file read-json))
  (define params (hash-ref json 'params))
  (define get-param (λ (k) (hash-ref params k)))
  (rule-manifest (let ([p0 (string->path (get-param 'outfile))])
                   (let-values ([(d f _) (split-path p0)])
                    (path->string (path-replace-suffix f ""))))
                 (get-param 'iters)
                 (get-param 'variables)
                 (get-param 'num_fuzz)
                 (get-param 'do_final_run)
                 (json->rules (hash-ref json 'eqs))))

(define (read-cache manifest)
  (define file-name (format "~a.json" (rule-manifest-name manifest)))
  (define file-path (build-path cache-dir file-name))
  (cond
   [(not (directory-exists? cache-dir)) #f]
   [(not (file-exists? file-path)) #f]
   [else
    (define cached (json->manifest file-path))
    (and (rule-manifest=? manifest cached) cached)]))
    
(define (clear-cache)
  (when (directory-exists? cache-dir)
    (delete-directory/files cache-dir)))

(define (make-cache-directory)
  (unless (directory-exists? cache-dir)
    (make-directory cache-dir)))
