#lang racket
(require graphics/graphics)
(require "complex-res.rkt")
(provide make-julia)
  

(define (has-escaped z lim)
  (let* ((r (get-real z))
         (i (get-imag z))
         (q (+ (* (- r (/ 1 4)) (- r (/ 1 4))) (* i i))))
    (cond ;((> (* q (+ q (- r (/ 1 4)))) (* i i (/ 1 4))) #t)
        ;  ((> (+ (* (+ r 1) (+ r 1)) (* i i)) (/ 1 16)) #t)
          ((> (mag-complex z) (* lim lim)) #t)
          (else #f)))
  )

(define (func z c)
  (add-complex (exp-complex z 2) c)
  )

(define (for-each-pixel x y window width height depth color-list limit zoom c hi-time)

  (define time (current-inexact-milliseconds))
  
  (define z (zoom x y width height))

  (define (iter z n)
    (define r (car z))
    (define i (cdr z))
    (cond ((> n depth) #f)
          ((> (+ (* r r) (* i i)) (* limit limit)) (- n 1))
          (else (iter (func z c) (+ n 1)))
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
          ((<= index 0) (set! color (car colors)))
          (else (get-color (- index 1) (cdr colors))))
    )

  (if color-index
      (get-color color-index color-list)
      '())
  
  ((draw-pixel window) (make-posn x y) color)
  
  (if (= x 0)
      ((draw-pixel window) (make-posn x y) (make-rgb 1 0 0))
      '())
  (cond ((= x width) (for-each-pixel 0 (+ y 1) window width height depth color-list limit zoom c hi-time))
        ((= y height) '())
        (else (for-each-pixel (+ x 1) y window width height depth color-list limit zoom c hi-time)))
  )

(define (make-julia window width height zoom depth colors limit c )
  (display c)
  (newline)
  (for-each-pixel 0 0 window width height depth colors limit zoom c 0)
  )