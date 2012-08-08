#lang racket

(require graphics/graphics)
(provide display-logistic-map)

(define MIN-R 0)
(define MAX-R 4)
(define R-STEP 0.0005)

(define GENERATIONS 250)

(define (display-logistic-map WIDTH HEIGHT viewport pix)
  (letrec ((gen-step (/ WIDTH GENERATIONS))
           (x-step (/ 1 HEIGHT))
           (black (make-rgb 0 0 0))
           (yellow (make-rgb 1 1 0))
           (init 0.5)
           (draw-r (lambda (r)
                     ((draw-viewport pix) black)
                     (define (draw-graph x gen)
                       (let ((new-gen (* r gen (- 1 gen))))
                         (if (>= x WIDTH)
                             '()
                             (begin
                               (let ((do-color (lambda (x)
                                                 (cond ((> x 1) (set! x 1))
                                                       ((< x -1) (set! x -1)))
                                                 (if (< 0 x) 
                                                     (make-rgb (- 1 x) 1 0)
                                                     (make-rgb 1 (+ 1 x) 0)))))
                                 ((draw-line pix) (make-posn x (- HEIGHT (* gen HEIGHT))) 
                                                  (make-posn (+ x gen-step) (- HEIGHT (* new-gen HEIGHT))) 
                                                  (do-color (- gen new-gen)))
                                 (draw-graph (+ x gen-step) new-gen))))
                         ))
                     (draw-graph 0 init)
                     (copy-viewport pix viewport)
                     (if (> r 4)
                         '()
                         (draw-r (+ r R-STEP)))
                     )))
    (draw-r 0)))
                  
    