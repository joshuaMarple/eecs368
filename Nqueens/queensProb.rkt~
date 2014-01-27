#lang racket
(require racket/trace)
(define (iperm s a)
  (if (null? s)
      (list a)
  (append-map(λ(x) (if (diag? x a) '() (iperm (remove x s) (cons x a)))) s)))

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

(define (nmapper n length) 
  (if (> length 0)
  (cons n (nmapper n (- length 1))) '()))

(define (diagHelper? new check g) (if (or (= (+ new g) check) (= (- new g) check)) #t #f))

(define (diag? n l) (ormap diagHelper? (nmapper n (length l)) l (enumerate-interval 1 (length l))))
                     
(diag? 4 '(8 6 2 7 1 3 5))

(define (permutations s)
  (if (null? s)
      ; empty set?
      (list '())
      ; sequence containing empty set
      (append-map (λ (x)
                 (map (λ (p) (cons x p))
                      (permutations (remove x s))))
               s)))

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

