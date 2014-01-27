#lang racket
(require racket/trace)

(define (iqueens s a)
  (if (null? s)
      (list a)
  (append-map(λ(x) (if (diag? x a) '() (iqueens (remove x s) (cons x a)))) s)))

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

(define (diag? n l)
  (ormap (λ(x y) (if (or (= (+ n y) x) (= (- n y) x))
      #t
      #f)) l (enumerate-interval 1 (length l))))

(define (q n)
  (displayln 
   (length (iqueens (enumerate-interval 1 n) '()))))

<<<<<<< HEAD
(length (iperm (enumerate-interval 1 2) '()))
(length (iperm (enumerate-interval 1 3) '()))
(length (iperm (enumerate-interval 1 4) '()))
(length (iperm (enumerate-interval 1 5) '()))
(length (iperm (enumerate-interval 1 6) '()))
(length (iperm (enumerate-interval 1 7) '()))
(length (iperm (enumerate-interval 1 8) '()))
(length (iperm (enumerate-interval 1 9) '()))
(length (iperm (enumerate-interval 1 10) '()))
(length (iperm (enumerate-interval 1 11) '()))
(length (iperm (enumerate-interval 1 12) '()))
(length (iperm (enumerate-interval 1 13) '()))
(length (iperm (enumerate-interval 1 14) '()))
(length (iperm (enumerate-interval 1 15) '()))
(length (iperm (enumerate-interval 1 16) '()))
(length (iperm (enumerate-interval 1 17) '()))
(length (iperm (enumerate-interval 1 18) '()))
(length (iperm (enumerate-interval 1 19) '()))
(length (iperm (enumerate-interval 1 20) '()))
(length (iperm (enumerate-interval 1 21) '()))
(length (iperm (enumerate-interval 1 22) '()))
(length (iperm (enumerate-interval 1 23) '()))
(length (iperm (enumerate-interval 1 24) '()))
(length (iperm (enumerate-interval 1 25) '()))
(length (iperm (enumerate-interval 1 26) '()))
(length (iperm (enumerate-interval 1 27) '()))
(length (iperm (enumerate-interval 1 28) '()))
(length (iperm (enumerate-interval 1 29) '()))

=======
(define (test t)
  (for-each q (enumerate-interval 1 t)))
>>>>>>> 55e5aeac802251a6b10067549705b4bf91ceae47

(test 20)
