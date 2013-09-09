#lang racket
(require racket/trace)
(define (iperm s a)
  (if (null? s)
      (list a)
  (append-map(λ(x) iperm (remove x s) (cons x a)) s)))
(trace iperm)
(iperm '() '(1 2))

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))
(define (nmapper n length) 
   (cons n (nmapper n (- length 1))))
(define (diagHelper? new check g) (if (or (= (+ new g) check) (= (- new g) check)) #t #f))
(define (diag? n l) (ormap diagHelper? () l (enumerate-interval 1 (length l))))
                     
             
;;(diagHelper? 4 3 2)
(diag? 1 '(2 3 4))

(define (permutations s)
  (if (null? s)
      ; empty set?
      (list '())
      ; sequence containing empty set
      (append-map (λ (x)
                 (map (λ (p) (cons x p))
                      (permutations (remove x s))))
               s)))



;;(trace enumerate-interval)
;(enumerate-interval 5 25)
;;(permutations (enumerate-interval 1 4))

;(define (diag? l1 l2) (define (diagHelp n l g) (print n)) map(λ (number1 number2)
;         (+ number1 number2)
;       l1
;       l2))
;
;(map (lambda (number1 number2)
;         (+ number1 number2))
;
;
;(diag? (enumerate-interval 1 4) '(2 3 4 5))

