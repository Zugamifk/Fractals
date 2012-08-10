#lang racket
(require graphics/graphics)
(require "complex-res.rkt")
(provide make-julia-ot)

; Pickover Stalks
 (define (pickover distance r i)
      (min (abs r) (abs i) distance))

 (define (orbit-x distance i)
      (min (abs i) distance))
 
; Orbit Trap
 (define (origin-orbit distance z)
   (define d (mag-complex z))
      (cond ((< d distance) d)
            (else distance)))

; the recursive function for the julia set
(define (func z c)
  (add-complex (exp-complex z 2) c)
  )

; Find the log base b of n
(define (log-b b n)
  (max 0 (/ (log n) (log b)))
  )
  
; main generating function
(define (for-each-pixel x y window width height depth color-func limit zoom c )
  
  ; Get the coordinates of Z in pixel space
  (define z (zoom x y width height))

  ; iterate the recurion until it escapes, to a maximum of depth times
  (define (iter z n distance)
    (define r (car z))
    (define i (cdr z))

    (cond ((> n depth) distance)
          ;((< n 10) (iter (func z c) (+ n 1) (origin-orbit distance z)))
          ((> (+ (* r r) (* i i)) (* limit limit)) distance)
          (else (iter (func z c) (+ n 1) (origin-orbit distance z)))
          )
    )

  ; Get the color of the pixel
  (define dis (/ (iter z 0 2) 2))
  ;(define col (- 1 (/ (+ (log-b 10 (* dis 5)) 1) 2)))
  (display dis)
  (newline)
  (define color (color-func dis)) 
  
  
  ; Draw the pixel
  ((draw-pixel window) (make-posn x y) color)
  
  ; iterate for each pixel
  (if (= x 0)
      ((draw-pixel window) (make-posn x y) (make-rgb 1 0 0))
      '())
  (cond ((= x width) (for-each-pixel 0 (+ y 1) window width height depth color-func limit zoom c))
        ((= y height) '())
        (else (for-each-pixel (+ x 1) y window width height depth color-func limit zoom c)))
  )

(define (make-julia-ot window width height zoom depth color-func limit c )
  (display c)
  (newline)
  (for-each-pixel 0 0 window width height depth color-func limit zoom c)
  )