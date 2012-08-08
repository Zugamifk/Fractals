#lang racket

(require "random/sierpinski.rkt")

(provide generate-random-fractal)

(define (generate-random-fractal window width height frac-type args)
  (case frac-type
    ('sierpinski (generate-sierpinski window width height (car args) (cadr args) (caddr args) (cdddr args)))
    )
  )