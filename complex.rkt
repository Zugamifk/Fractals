#lang racket

(require "complex/mandelbrot.rkt")
(require "complex/julia.rkt")
(require "complex/julia-orbit-trap.rkt")
(require "complex/julia-orbit-trap-ss.rkt")
(provide generate-complex-fractal)
(define LIMIT 2)
(define (generate-complex-fractal window width height zoom frac-type args)
  (case frac-type
    ('mandelbrot (make-mandelbrot window width height (car args) (cadr args) LIMIT))
    ('julia (make-julia window width height zoom (car args) (cadr args) LIMIT (caddr args)))
    ('julia-orbit-trap (make-julia-ot window width height zoom (car args) (cadr args) LIMIT (caddr args)))
    ('julia-orbit-trap-ss (make-julia-ot-ss window width height zoom (car args) (cadr args) LIMIT (caddr args) (cadddr args)))
    )
  )