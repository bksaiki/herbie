#lang racket

(require pkg/lib json)

(provide (struct-out rule-manifest) write-cache read-cache clear-cache)

(struct rule-manifest (name iters varc fuzzc final? rules))

(define cache-dir (build-path (pkg-directory "ruler-herbie") "cache"))

(define (manifest->json manifest)
  (make-hash
    `((name . ,(rule-manifest-name manifest))
      (iters . ,(~a (rule-manifest-iters manifest)))
      (varc . ,(~a (rule-manifest-varc manifest)))
      (fuzzc . ,(~a (rule-manifest-fuzzc manifest)))
      (final . ,(~a (rule-manifest-final? manifest)))
      (rules . ,(~a (rule-manifest-rules manifest))))))

(define (json->manifest file)
  (define json (call-with-input-file file read-json))
  (define parse-string (λ (s) (if s (call-with-input-string s read) #f)))
  (define get (λ (k) (hash-ref json k)))
  (rule-manifest (get 'name)
                 (parse-string (get 'iters))
                 (parse-string (get 'varc))
                 (parse-string (get 'fuzzc))
                 (parse-string (get 'final))
                 (parse-string (get 'rules))))

(define (write-cache manifest)
  (define file-name (format "~a.json" (rule-manifest-name manifest)))
  (unless (directory-exists? cache-dir)
    (make-directory cache-dir))
  (call-with-output-file (build-path cache-dir file-name)
    #:exists 'replace
    (λ (o) (write-json (manifest->json manifest) o))))

(define (read-cache manifest)
  (define file-name (format "~a.json" (rule-manifest-name manifest)))
  (define file-path (build-path cache-dir file-name))
  (cond
   [(not (directory-exists? cache-dir)) #f]
   [(not (file-exists? file-path)) #f]
   [else
    (define cached (json->manifest file-path))
    (cond
     [(and (equal? (rule-manifest-iters manifest) (rule-manifest-iters cached))
           (equal? (rule-manifest-varc manifest) (rule-manifest-varc cached))
           (equal? (rule-manifest-fuzzc manifest) (rule-manifest-fuzzc cached))
           (equal? (rule-manifest-final? manifest) (rule-manifest-final? cached)))
      cached]
     [else #f])]))
    
(define (clear-cache)
  (when (directory-exists? cache-dir)
    (delete-directory/files cache-dir)))
