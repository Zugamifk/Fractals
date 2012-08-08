#lang racket
(require graphics/graphics)
(require "complex-res.rkt")
(provide make-mandelbrot)

(define (to-complex x y w h)
  (let ((div-x (/ w 2))
        (div-y (/ h 2)))
    (make-complex (/ (- x div-x) (/ div-x 2))
                  (/ (- y div-y) (/ div-y 2)))
    )
  )
  

(define (has-escaped z lim)
  (let* ((r (get-real z))
         (i (get-imag z))
         (q (+ (* (- r (/ 1 4)) (- r (/ 1 4))) (* i i))))
    (cond ;((> (* q (+ q (- r (/ 1 4)))) (* i i (/ 1 4))) #t)
        ;  ((> (+ (* (+ r 1) (+ r 1)) (* i i)) (/ 1 16)) #t)
          ((> (mag-complex z) (* lim lim)) #t)
          (else #f)))
  )

(define (for-each-pixel x y window width height depth color-list limit hi-time)
  
  (define time (current-inexact-milliseconds))
  
  (define c (to-complex x y width height))
  (define z (make-complex 0 0))
  (display z) 
  (display c)
  (newline)
  (define (iter z n)
    (define r (car z))
    (define i (cdr z))
    (cond ((> n depth) #f)
          ((> (+ (* r r) (* i i)) (* limit limit)) (- n 1))
          (else (iter (cons (+ (- (* r r) (* i i)) (car c))
                                      (+ (* 2 r i) (cdr c)))
                        (+ n 1)))
          )
    )

  (define color-index (iter z 0))
  (define color (make-rgb 0 0 0)) 
  
  (set! time (- (current-inexact-milliseconds) time))
  (cond ((> time hi-time) (begin
                            (set! hi-time time)
                            (display hi-time)
                            (newline))))
  
  
  (define (get-color index colors)
    (cond ((null? colors) (get-color index (cdr color-list)))
          ((= index 0) (set! color (car colors)))
          (else (get-color (- index 1) (cdr colors))))
    )
  
  (if color-index
      (get-color color-index color-list)
      '())
  
  ((draw-pixel window) (make-posn x y) color)

  (cond ((= x width) (for-each-pixel 0 (+ y 1) window width height depth color-list limit hi-time))
        ((= y height) '())
        (else (for-each-pixel (+ x 1) y window width height depth color-list limit hi-time)))
  )

(define (make-mandelbrot window width height depth colors limit)
  (for-each-pixel 0 0 window width height depth colors limit 0)
  )