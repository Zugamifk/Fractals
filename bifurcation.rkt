#lang racket

(require graphics/graphics)
(provide display-bifurcation)

(define N 50)
(define tolerance 1000)

(define (eq-with-tolerance? a b)
  (= a b))
    ; (floor (* b tolerance))))

(define (recur r x)
  (* r x (- 1 x)))

(define (has-cycle? l x)
  (define (iter l c)
    (if (eq-with-tolerance? x (car l))
        c
        (if (null? (cdr l))
            #f
            (iter (cdr l) (cons (car l) c)))))
  (iter l (list (car l))))

(define (display-bifurcation WIDTH HEIGHT window pix)
  ((draw-viewport pix) (make-rgb 1 1 1))
  (letrec ((r-step (/ WIDTH 4))
           (x-step HEIGHT)
           (white (make-rgb 1 1 1))
           (black (make-rgb 0 0 0))
           (iter (lambda (r)
                   (letrec ((gen-x (lambda (x n)
                                     (if (> 0 n)
                                         (gen-x (recur r x) (- n 1))
                                         x)))
                            (first (gen-x 0.5 N))
                            (find-cycle (lambda (l x)
                                          (let ((c (has-cycle? l x)))
                                            (if c
                                                c
                                                (find-cycle (cons x l) (recur r x))))))
                            (cycle (find-cycle (list first) (recur r first)))
                            (draw (lambda (l)
                                    (if (null? l)
                                        '()
                                        (begin ((draw-pixel pix) (make-posn (* r r-step)
                                                                    (* x-step (car l)))
                                                          black)
                                               (draw (cdr l)))))))
                     (display (recur r first))
                     (display " -> ")
                     (display cycle)
                     (draw cycle)
                     (newline)
                     (copy-viewport pix window))
                   (if (> r 4)
                       '()
                       (iter (+ r (/ 4 WIDTH)))))))
    (iter 1)))
                                     
                   
    