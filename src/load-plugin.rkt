#lang racket
(require setup/getinfo racket/runtime-path)
(require (submod "interface.rkt" internals))
(provide load-herbie-plugins load-herbie-builtins)

(define-runtime-module-path bool-plugin     "reprs/bool.rkt")
(define-runtime-module-path binary32-plugin "reprs/binary32.rkt")
(define-runtime-module-path binary64-plugin "reprs/binary64.rkt")
(define-runtime-module-path fallback-plugin "reprs/fallback.rkt")

(define (load-herbie-builtins)
  ;; Warning: the order here is important!
  (dynamic-require bool-plugin #f)
  (dynamic-require binary64-plugin #f)
  (dynamic-require binary32-plugin #f)
  (dynamic-require fallback-plugin #f))


;; loads builtin representations as needed
;; usually if 'load-herbie-plugins' has not been called
(define (generate-builtins name)
  (match name
   ['bool     (dynamic-require bool-plugin #f) #t]
   ['binary64 (dynamic-require binary64-plugin #f) #t]
   ['binary32 (dynamic-require binary32-plugin #f) #t]
   ['racket   (dynamic-require fallback-plugin #f) #t]
   [_ #f]))

(register-generator! generate-builtins)


;; Gets the path of the executable
(define (get-exe-directory)
  (define-values (base name dir?)
    (split-path (path->complete-path (find-system-path 'run-file))))
  base)

(define plugin-directory
  (simplify-path
    (build-path (get-exe-directory)
                (case (system-type)
                 [(windows) "plugins/"]
                 [else "../plugins/"]))))

(define (get-subdirectories dir)
  (filter-map
      (λ (name) (let ([sub-fd (build-path dir name)])
                  (and (directory-exists? sub-fd) sub-fd)))
      (directory-list dir)))

;; Loads Herbie plugins locally, assuming from `../plugins/` (or `plugins/` on Windows)
;; relative to the executable. Assumes running a standalone executable created by
;; `raco exe` and `raco distribute`.
(define (load-local-herbie-plugins)
  (for ([subdir (in-list (get-subdirectories plugin-directory))])
    (define info
      (with-handlers ([exn:fail:filesystem? (const #f)])
        (get-info/full subdir)))
    (define value (info 'herbie-plugin (const false)))
    (when value
      (let ([main (build-path subdir "main.rkt")])
        (dynamic-require main #f)))))

;; loads Herbie plugins using information from `raco setup`
(define (load-herbie-plugins-from-collects)
  (for ([dir (find-relevant-directories '(herbie-plugin))])
      (define info
        (with-handlers ([exn:fail:filesystem? (const false)])
          (get-info/full dir)))
      (define value (info 'herbie-plugin (const false)))
      (when value
        (with-handlers ([exn:fail:filesystem:missing-module? void])
          (dynamic-require value #f)))))

;; loads all Herbie plugins (builtin / and user-installed)
(define (load-herbie-plugins)
  (load-herbie-builtins)    ; automatically load default representations
  (if (null? (current-library-collection-paths))  ;; most-likely running in standalone executable
      (load-local-herbie-plugins)
      (load-herbie-plugins-from-collects)))
