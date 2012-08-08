#lang racket

(require "random-res.rkt")
(require graphics/graphics)
(provide generate-sierpinski)

(define (fill-random window dim prob colour n)
  (define (iter n count)
    (let* ((rand-x (+ (get-x dim) (random (get-w dim))))
           (rand-y (+ (get-y dim) (random (get-h dim))))
           (curr-quad (get-quad rand-x rand-y dim))
           (draw? (< (random 100) (get-prob curr-quad prob)))
           )
      (cond (draw?
             (let ((pos (make-posn rand-x rand-y)))
               ((draw-pixel window) pos colour)
               (set! count (+ count 1)))
             )))
     (if (> n 0)
        (iter (- n 1) count)
        '())
    )
  (iter n 0)
 )
  
(define (new-scale scale)
  (* scale scale))

(define (fill-random-fractal window dim prob n depth color-scale n-scale)
  
  (cond ((> depth 0)
      (let* ((x (get-x dim))
            (y (get-y dim))
            (w (get-w dim))
            (h (get-h dim))
            (q1-dim (make-dim (+ x (round (/ w 2))) y (round (/ w 2)) (round (/ h 2))))
            (q2-dim (make-dim x y (round (/ w 2)) (round (/ h 2))))
            (q3-dim (make-dim x (+ y (round (/ h 2))) (round (/ w 2)) (round (/ h 2))))
            (q4-dim (make-dim (+ x (round (/ w 2))) (+ y (round (/ h 2))) (round (/ w 2)) (round (/ h 2))))
            (colour (make-rgb (- 1 color-scale) (- 1 color-scale) color-scale)) 
            )
        (fill-random window dim prob colour n)
        (fill-random-fractal window q1-dim (scale-prob prob (get-q1 prob)) (* n n-scale) (- depth 1) (new-scale color-scale) n-scale)
        (fill-random-fractal window q2-dim (scale-prob prob (get-q2 prob)) (* n n-scale) (- depth 1) (new-scale color-scale) n-scale)
        (fill-random-fractal window q3-dim (scale-prob prob (get-q3 prob)) (* n n-scale) (- depth 1) (new-scale color-scale) n-scale)
        (fill-random-fractal window q4-dim (scale-prob prob (get-q4 prob)) (* n n-scale) (- depth 1) (new-scale color-scale) n-scale))
      ))
  )

(define (generate-sierpinski window width height prob n depth args)
  (fill-random-fractal window
                       (make-dim 0 0 width height)
                       prob
                       n
                       depth
                       (car args)
                       (cadr args))
  )