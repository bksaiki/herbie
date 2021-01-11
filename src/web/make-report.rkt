#lang racket

(require json (only-in xml write-xexpr))
(require "../common.rkt" "../config.rkt" "../datafile.rkt" "../interface.rkt"
         "../pareto.rkt" "../sandbox.rkt" "common.rkt" "plot.rkt")

(provide make-report-page)

(define (badge-label result)
  (match (table-row-status result)
    ["error" "ERR"]
    ["crash" "!!!"]
    ["timeout" "TIME"]
    [_ (format-bits (- (table-row-start result) (table-row-result result)) #:sign #t)]))

(define (pareto->json tests pareto-points out)
  (define h (make-hasheq))
  (define pareto-points* (map (λ (p) (list (car p) (cdr p))) pareto-points))
  (define ymax (apply + (map (compose representation-total-bits get-representation table-row-precision) tests)))
  (hash-set! h 'points pareto-points*)
  (hash-set! h 'branch *herbie-branch*)
  (hash-set! h 'y-max ymax)
  (write-json h out))

(define (make-report-page out info dir)
  (match-define (report-info date commit branch hostname seed flags points iterations note tests) info)

  (define-values (costs times)
    (for/fold ([costs '()] [times '()]) ([test tests]
              #:unless (null? (table-row-cost&time test)))
      (let ([ct (table-row-cost&time test)])
        (values (append (car ct) costs) (append (cdr ct) times)))))


  ;(if (> (length costs) 1) ; generate the scatterplot if necessary
  ;    (call-with-output-file (build-path dir "scatterplot.png")
  ;      (λ (out) (make-cost-scatter-plot (cons costs times) out)) #:exists 'replace)
  ;    (when (file-exists? (build-path dir "scatterplot.png"))
  ;      (delete-file (build-path dir "scatterplot.png"))))

  (define cost&accuracy (map table-row-cost&accuracy tests))
  (define pareto-points (compute-pareto-curve cost&accuracy))
  (cond
   [(> (length pareto-points) 1) ; generate the scatterplot if necessary
    (call-with-output-file (build-path dir "cost-accuracy.png")
      (λ (out) (make-alt-cost-accuracy-plot tests pareto-points out)) #:exists 'replace)
    (call-with-output-file (build-path dir "pareto.json")
      (λ (out) (pareto->json tests pareto-points out)) #:exists 'replace)]
   [else
    (when (file-exists? (build-path dir "cost-accuracy.png"))
      (delete-file (build-path dir "cost-accuracy.png")))
    (when (file-exists? (build-path dir "pareto.json"))
      (delete-file (build-path dir "pareto.json")))])

  (define table-labels
    '("Test" "Start" "Result" "Target" "Time"))

  (define help-text
    #hash(("Result" . "Color key:\nGreen: improved accuracy\nLight green: no initial error\nOrange: no accuracy change\nRed: accuracy worsened")
          ("Target" . "Color key:\nDark green: better than target\nGreen: matched target\nOrange: improved but did not match target\nYellow: no accuracy change\n")))

  (define total-time (apply + (map table-row-time tests)))
  (define total-passed
    (for/sum ([row tests])
      (if (member (table-row-status row) '("gt-target" "eq-target" "imp-start")) 1 0)))
  (define total-available
    (for/sum ([row tests])
      (if (not (equal? (table-row-status row) "ex-start")) 1 0)))
  (define total-crashes
    (for/sum ([row tests])
      (if (equal? (table-row-status row) "crash") 1 0)))

  (define total-gained
    (for/sum ([row tests])
      (or (table-row-result row) 0)))
  (define total-start
    (for/sum ([row tests])
      (or (table-row-start row) 0)))

  (define (round* x)
    (inexact->exact (round x)))

  (define sorted-tests
    (sort (map cons tests (range (length tests))) >
          #:key (λ (x) (or (table-row-start (car x)) 0))))

  (define classes
    (if (ormap table-row-target tests) '(no-target) '()))

  ;; HTML cruft
  (fprintf out "<!doctype html>\n")
  (write-xexpr
   `(html
     (head
      (title "Herbie results")
      (meta ((charset "utf-8")))
      (link ((rel "stylesheet") (type "text/css") (href "report.css")))
      (script ((src "report.js")))
      (script ((src "http://d3js.org/d3.v3.min.js") (charset "utf-8")))
      (script ((type "text/javascript") (src "arrow-chart.js"))))
 
     (body
      (nav ([id "links"])
       (div
        (a ([href "timeline.html"]) "Metrics"))
       (div
        (a ([href "#about"]) "Flags")
        (a ([href "#results"]) "Results")))

      (div ((id "large"))
       ,(render-large "Time" (format-time total-time))
       ,(render-large "Passed" (~a total-passed) "/" (~a total-available))
       ,(if (> total-crashes 0) (render-large "Crashes" (~a total-crashes)) "")
       ,(render-large "Tests" (~a (length tests)))
       ,(render-large "Bits" (~a (round* (- total-start total-gained))) "/" (~a (round* total-start))))

      (figure (svg ((id "graph") (class "arrow-chart") (width "400"))))

     (ul ((id "test-badges"))
      ,@(for/list ([(result id) (in-dict sorted-tests)])
          `(li ((class ,(format "badge ~a" (table-row-status result)))
                (title ,(format "~a (~a to ~a)"
                                (table-row-name result)
                                (format-bits (table-row-start result))
                                (format-bits (table-row-result result))))
                (data-id ,(~a id)))
               ,(badge-label result))))
     (hr ((style "clear:both;visibility:hidden")))

     (table ((id "results") (class ,(string-join (map ~a classes) " ")))
      (thead
       (tr ,@(for/list ([label table-labels])
               (if (dict-has-key? help-text label)
                   `(th ,label " " (span ([class "help-button"] [title ,(dict-ref help-text label)]) "?"))
                   `(th ,label)))))
      (tbody
       ,@(for/list ([result tests] [id (in-naturals)])
           `(tr ((class ,(~a (table-row-status result))))
                (td ,(or (table-row-name result) ""))
                (td ,(format-bits (table-row-start result)))
                (td ,(format-bits (table-row-result result)))
                (td ,(format-bits (table-row-target result)))
                (td ,(format-time (table-row-time result) #:min 1000))
                ,(if (table-row-link result)
                     `(td
                       (a ((id ,(format "link~a" id))
                           (href ,(format "~a/graph.html" (table-row-link result))))
                          "»"))
                     "")))))

; Main scatterplot
;      ,(if (> (length costs) 1)
;         `(div ([id "scatterplot"] [style "margin-top: 2.5em"])
;             (img ([width "800"] [height "300"] [title "cost-scatter"]
;                   [data-name "Cost Scatter"] [src "scatterplot.png"])))
;           "")))
      ; Cost accuracy scatter
      ,(if (> (length costs) 1)
         `(div ([id "scatterplot"] [style "margin-top: 2.5em"])
             (img ([width "800"] [height "300"] [title "cost-accuracy"]
                   [data-name "Cost Accuracy"] [src "cost-accuracy.png"]))
             (p ,(format "Pareto measure: ~a" (pareto-measure-pnts pareto-points))))
           "")))
   out))
