#lang racket

(provide make-complex 
         get-real 
         get-imag 
         conj-complex 
         add-complex 
         sub-complex 
         mul-complex 
         div-complex
         exp-complex
         mag-complex)

(define (make-complex real imaginary)
  (cons real imaginary))
(define (get-real num)
  (car num))
(define (get-imag num)
  (cdr num))

(define (conj-complex n)
  (make-complex (get-real n)
                (* (get-imag n) -1))
  )
(define (add-complex n1 n2)
  (make-complex (+ (get-real n1) (get-real n2))
                (+ (get-imag n1) (get-imag n2)))
  )
(define (sub-complex n1 n2)
  (make-complex (- (get-real n1) (get-real n2))
                (- (get-imag n1) (get-imag n2)))
  )
(define (mul-complex n1 n2)
  (let ((a (get-real n1))
        (b (get-imag n1))
        (c (get-real n2))
        (d (get-imag n2)))
        (make-complex (- (* a c) (* b d))
                      (+ (* b c) (* a d))))
  )
(define (div-complex n1 n2)
  (make-complex (/ (+ (* (get-real n1) (get-real n2)) (* (get-imag n1) (get-imag n2)))
                   (+ (* (get-real n2) (get-real n2)) (* (get-imag n2) (get-imag n2))))
                (/ (- (* (get-imag n1) (get-real n2)) (* (get-real n1) (get-imag n2)))
                   (+ (* (get-real n2) (get-real n2)) (* (get-imag n2) (get-imag n2))))
                )
  )
(define (exp-complex z e)
  (if (= e 1)
      z
      (mul-complex z (exp-complex z (- e 1)))
      )
  )

(define (mag-complex n)
  (+ (* (get-real n) (get-real n)) (* (get-imag n) (get-imag n)))
  )