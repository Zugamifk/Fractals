#!racket

(require graphics/graphics)
(require "fractal-index.rkt")
(require "colors.rkt")
(require "complex/complex-res.rkt")
(require "logistic-map.rkt")
(require "bifurcation.rkt")

(define port (open-input-file "pts.csv"))
(port-count-lines! port)
(define (rp) (random 9550))
(define (get-vals)
  (define (iter n)
    (cond ((eof-object? port) (get-vals))
          ((= n 0) (read-line port))
          (else (begin 
                  (read-line port)
                  (iter (- n 1))))))
  (iter (rp)))

(display (get-vals))

(define DEPTH 1000)
(define ZOOM 0.7)
;(define colors-list (generate-2-color DEPTH (make-rgb 0.8 0.5 0.02) (make-rgb 0.52 0.15 0.08 )))
(define color-func (circles summer 6));(make-rgb 1 1 0) (make-rgb 0.52 0.15 0.08 )))


; Probabilities, Number of dots per iteration, Number of Iterations, Color scale, Probability scale
(define sample-sierpinski-args (list (list 0 100 100 100) 10 10 0.8 0.99))

; Number of iterations, List of colors, Distance breakoff limit
;(define sample-mandelbrot-args (list DEPTH colors-list))

; Number of iterations, List of colors, Distance breakoff limit
;(define sample-julia-args (list DEPTH colors-list (cons -0.533 0.524)));(random-complex 100)(cons 0.369269 0.152416)
(define sample-julia-args (list DEPTH color-func (cons (/ -998 807) (/ 23 269)) 4))
; Cool Julia Sets
;(define sample-julia-args (list DEPTH colors-list (cons 0.369269 0.152416)))


(define WIDTH 1650)
(define HEIGHT 1024)

(define (graphics-errors err)
  (cond ((eq? err 'graphics-not-open)
         (error "Graphics failed to open"))
        )
  )

(define (error-on-fail test error-symbol)
  (if (test)
      #t
      (graphics-errors error-symbol)
      )
  )

(define (set-zoom zoom)
  (lambda (x y w h)
    (let (
          (hr (/ h w))
          (wr (/ w h))
          (div-x (* w 0.5))
          (div-y (* h 0.5)))
      (make-complex (* (- x div-x) (/ 2 div-x ) zoom (max wr 1))
                    (* (- y div-y) (/ 2 div-y ) zoom (max hr 1)))
    ))
  )

(define make-window
  (lambda ()
    (open-graphics)    
    (error-on-fail graphics-open? 'graphics-not-open)
    
    (define window (open-viewport "Fractal Test Window" WIDTH HEIGHT))
    (define pix (open-pixmap "test" WIDTH HEIGHT))

    ((flip-viewport window))
    
    (generate-fractal window WIDTH HEIGHT (set-zoom ZOOM) 'complex 'julia-orbit-trap-ss sample-julia-args)
    
   ; (copy-viewport pix window)
    ;(close-graphics)
    )
  )

(make-window)