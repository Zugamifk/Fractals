#lang racket

(require graphics/graphics)
(require plot/utils)
(provide to-rgb 
         average-colors
         rainbow-colors
         primaries
         generate-log-to-green
         generate-linear
         generate-rainbow-cycle
         generate-to-black
         generate-2-color
         lerp
         plerp
         multi-lerp
         multi-lerp-deets
         circles
         natural
         autumn
         blue-pink
         summer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (to-rgb r g b)
  (make-rgb (/ r 255)
            (/ g 255)
            (/ b 255)))

(define (from-hex r1 r2 g1 g2 b1 b2)
  (define (from n1 n2)
    (+ (* n1 16) n2))
  (to-rgb (from r1 r2)
          (from g1 g2)
          (from b1 b2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define primaries (list
                   (make-rgb 0 0 0)
                   (make-rgb 0.8 0.2 0.2)
                   (make-rgb 0.6 0.4 0.2)
                   (make-rgb 0.4 0.6 0.2)
                   (make-rgb 0.2 0.8 0.2)
                   (make-rgb 0.2 0.6 0.4)
                   (make-rgb 0.2 0.4 0.6)
                   (make-rgb 0.2 0.2 0.8)
                   (make-rgb 0.4 0.2 0.6)
                   (make-rgb 0.6 0.2 0.8)))

(define rainbow-colors (list
                     (make-rgb 0 0 0)
                     (make-rgb 0.2 0.2 0.8)
                     (make-rgb 0.8 0.6 0.6)
                     (make-rgb 0.8 0.4 0.4)
                     (make-rgb 0.8 0.2 0.2)
                     (make-rgb 0.8 0.4 0.2)
                     (make-rgb 0.8 0.6 0.2)
                     (make-rgb 0.8 0.8 0.2)
                     (make-rgb 0.6 0.8 0.4)
                     (make-rgb 0.4 0.6 0.6)
                     (make-rgb 0.2 0.4 0.8)
                     (make-rgb 0.2 0.2 0.8)))

(define natural (list
             (from-hex 5 10 5 4 3 8)
             (from-hex 9 9 6 5 4 2)
             (from-hex 10 8 11 2 7 2)
             (from-hex 2 6 1 15 1 11)
             (from-hex 8 5 7 6 3 2)
             (from-hex 4 1 0 14 0 0)
           ;  (from-hex 11 15 12 6 12 12)
             ))
         
(define autumn (list
                (from-hex 4 1 0 14 0 0)
                (from-hex 9 10 2 12 0 0)
                (from-hex 5 5 2 9 0 10)
                (from-hex 2 4 1 0 0 4)
                (from-hex 15 10 12 5 5 2)
                (from-hex 15 15 8 7 0 0)
                (from-hex 9 9 6 1 1 3)
                ))
                
(define blue-pink (list
                (from-hex 2 5 3 9 4 4)
                (from-hex 3 1 5 12 7 8)
                (from-hex 9 10 10 2 12 13)
                (from-hex 3 5 4 6 5 2)
                (from-hex 14 2 13 7 13 13)
                (from-hex 15 15 4 3 5 11)
                (from-hex 2 4 2 12 3 0)
                ))

(define summer (list
                (from-hex 1 12 1 11 1 8)
                (from-hex 1 8 2 2 1 9)
                (from-hex 4 2 5 2 3 7)
                (from-hex 1 4 0 15 1 0)
                (from-hex 14 12 9 8 1 6)
                (from-hex 15 15 15 15 2 9)
                (from-hex 4 5 5 1 0 0)
                ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (clamp n)
  (cond ((= (max 0 n) 0) 0)
        ((= (min 1 n) 1) 1)
        (else n)))
(define (clamp-color r g b)
  (make-rgb (clamp r)
            (clamp g)
            (clamp b)))

(define (scale-color color scale)
  (make-rgb (* (rgb-red color) scale)
            (* (rgb-green color) scale)
            (* (rgb-blue color) scale))
  )

(define (add-colors color1 color2)
  (clamp-color (+ (rgb-red color1) (rgb-red color2))
               (+ (rgb-green color1) (rgb-green color2))
               (+ (rgb-blue color1) (rgb-blue color2)))
  )
  
(define (average-colors colors n) 
 (foldl add-colors (make-rgb 0 0 0) 
        (map (lambda (c)
               (scale-color c (/ 1 n)))
             colors))
  )

(define (print-color color)
  (if (null? color)
      '()
      (begin
        (display (rgb-red color))
        (display " : ")
        (display (rgb-green color))
        (display " : ")
        (display (rgb-blue color))
        (newline)))
  )
  
(define const
  (lambda (c)
    c))

(define (scale-change s)
  (lambda (c)
    (* s c)))

(define (exp-change s e)
  (define (pow n e total)
    (cond ((= e 0) 1)
          ((= e 1) n)
          (else (pow n (- e 1) (* total n)))))
  (lambda (c)
    ((scale-change (pow s e 1)) c))
  )

(define (reverse-exp s e)
  (define (pow n e)
    (cond ((= e 0) 1)
          ((= e 1) n)
          (else (* n (pow n (- e 1))))))
  (lambda (c)
    (- 1 ((scale-change (pow s e)) c)))
  )

(define (generate-next-step curr lr lg lb)
  (let ((r (car curr))
        (g (cadr curr))
        (b (caddr curr)))
        (list (lr r) (lg g) (lb b)))
  )
                                             
(define (generate-log-to-green n)
  (define (iter n colors curr)
;    (display curr)
;    (display " ")
;    (display n)
;    (newline)
    (cond ((= n 0) colors)
          (else (let* ((new (generate-next-step curr
                                                const
                                                (exp-change 0.996 (+ n 1))
                                                const))
                       (r (car new))
                       (g (cadr new))
                       (b (caddr new)))
                  (iter (- n 1)
                        (cons (make-rgb r g b) colors)
                        new)))))
  (iter n '() (list 0 1 0))
  )

(define (generate-cycle length start-color rl gl bl)
  (define (iter n colors curr)
    (display curr)
    (display " ")
    (display n)
    (newline)
    
    (cond ((= n 0) colors)
          (else (let* ((new (generate-next-step curr rl gl bl))
                       (r (car new))
                       (g (cadr new))
                       (b (caddr new)))
                  (iter (- n 1)
                        (cons (make-rgb r g b) colors)
                        new)))))
  (iter length '() start-color)
  )

(define (reverse-list l new)
 ; (display l)
;  (newline)
  (cond ((null? l) new)
        (else (reverse-list (cdr l) (cons (car l) new)))
        )
  )

(define (alter-list colors mod)
  (define (iter remaining new-list n)
    (cond ((null? remaining) new-list)
          (else (iter (cdr remaining) (cons (mod (car colors) n) new-list) (+ n 1)))
          )
    )
  (reverse-list (iter colors '() 0) '()))
                  

(define (generate-linear n color scale)
  (generate-cycle n color (scale-change scale) (scale-change scale) (scale-change scale))
  )

(define (generate-to-black n color scale)
  (reverse-list
   (generate-cycle n color (scale-change scale) (scale-change scale) (scale-change scale))
   '())
  )
  
(define (generate-2-color n color1 color2)
  (letrec ((step (/ 1 n))
           (combine (lambda (result mod)
                      (print-color (car result))
                      (if (< 1 mod)
                          result
                          (combine (cons (add-colors (scale-color color1 (- 1 mod))
                                                     (scale-color color2 mod))
                                         result)
                                   (+ mod step)
                                   )
                          ))))
    (combine (cons '() '()) step))
  )
                                         
    

(define (generate-rainbow-cycle n)
  (define (get-color n colors)
    (if (= n 0)
        (car colors)
        (get-color (- n 1) (cdr colors))
        )
    )
  (define (fill n l)
    (if (< n 0)
        l
        (fill (- n 1) (cons (get-color (remainder n 10) (cdr rainbow-colors)) l))
        )
    )
  
  (define i 0)
  (define (counter)
    (set! i (+ i 1)))
  
  (map (lambda (c)
         (counter)
         (make-rgb (* (- 1 (/ (- i 1) n)) (rgb-red c))
                   (* (- 1 (/ (- i 1) n)) (rgb-green c))
                   (* (- 1 (/ (- i 1) n)) (rgb-blue c))))
         (fill n '()))
  )
; =================================
; color lerping function, takes different args, returns a lambda
; ================================

(define (lerp color1 color2)
  (let ((red-inc (- (rgb-red color2) (rgb-red color1)))
         (blue-inc (- (rgb-blue color2) (rgb-blue color1)))
         (green-inc (- (rgb-green color2) (rgb-green color1))))
    (lambda (factor)
      (make-rgb
       (+ (rgb-red color1) (* red-inc factor))
       (+ (rgb-green color1) (* green-inc factor))
       (+ (rgb-blue color1) (* blue-inc factor)))
      )
    )
  )
            
(define (plerp color1 color2)
  (let ((red-inc (- (rgb-red color2) (rgb-red color1)))
         (blue-inc (- (rgb-blue color2) (rgb-blue color1)))
         (green-inc (- (rgb-green color2) (rgb-green color1))))
    (lambda (factor)
      (make-rgb
       (+ (rgb-red color1) (* red-inc factor factor))
       (+ (rgb-green color1) (* green-inc factor factor))
       (+ (rgb-blue color1) (* blue-inc factor factor)))
      )
    )
  )

(define (multi-lerp colors num)
  (define index-inc (/ 1 (- num 1)))
  (define (get-color n)
    (define (iter mul c)
      (if (>= mul n)
          c
          (iter (+ mul index-inc) (cdr c))))
    (iter index-inc colors)) 
  (lambda (factor)
    (display factor)
    (newline)
    (define (small-fac)
      (define (iter f)
        (if (> (+ f index-inc) factor)
            f
            (iter (+ f index-inc))))
      (* num (- factor (iter 0))))
    (define color (get-color factor))
    ((lerp (car color) (cadr color)) (small-fac))
    )
  )

(define (multi-lerp-deets colors num)
  (define index-inc (/ 1 (- num 1)))
  (define (get-color n)
    (define (iter mul c)
      (if (>= mul n)
          c
          (iter (+ mul index-inc) (cdr c))))
    (iter index-inc colors)) 
  (lambda (factor)
  ;  (display factor)
    (define (get-scale s f)
      (display ".")
      (cond ((= 1 s) 1)
            ((< f 0.0001) 1)
            ((< f index-inc) (get-scale (+ s 0.2) (/ f index-inc)))
            (else (begin
                    (set! factor f)
                    s))))
    (define color-scale (get-scale 0 factor))
  ;  (display " SCALED: ")
  ;  (display factor)
    (display " Color Scale: ")
    (display color-scale)
    
    (define (small-fac)
      (define (iter f)
        (if (> (+ f index-inc) factor)
            f
            (iter (+ f index-inc))))
      (* num (- factor (iter 0))))
  ;  (display " Small: ")
  ;  (display (small-fac))
    (newline)
    (define color (get-color factor))
    ((lerp (scale-color (car color) color-scale)
           (scale-color (cadr color) color-scale)) (small-fac))
    )
  )

(define (circles colors num)
  (define index-inc (/ 1 (- num 1)))
  (define min (expt 0.1 (- num 1)))
  (define (get-color c f)
    (cond
      ((null? c) (get-color colors f))
      ((< f min) (car colors))
      ((< f index-inc) (get-color (cdr c) (/ f index-inc)))
      (else (car c))))
  (lambda (factor)
    (get-color colors factor)
    )
  )