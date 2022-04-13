#lang racket

;   
; Acts as the entry point of the embedded executable using Herbie as a library.
;
; The `raco exe` tool complains about the use of plugins since the plugin
; interface is accessed by external plugins via `herbie/plugin` and
; internally by "plugin.rkt". Mixing library names and relative paths
; of the same module across module boundaries is not allowed.
; By importing the true entry point through `herbie/herbie`, the plugin interface
; is unambiguously set to `herbie/plugin`.
;

(require (submod herbie/herbie main))
