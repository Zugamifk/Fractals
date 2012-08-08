#lang racket

(require "random.rkt")
(require "complex.rkt")

(provide generate-fractal)

(define (generate-fractal window width height zoom alg-type frac-type args)
  (case alg-type
    ('random (generate-random-fractal window width height frac-type args))
    ('complex (generate-complex-fractal window width height zoom frac-type args))
    )
  )