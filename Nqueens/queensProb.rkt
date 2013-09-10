#lang racket
(require racket/trace)
(define (iperm s a)
  (if (null? s)
      (list a)
  (append-map(λ(x) iperm (remove x s) (cons x a)) s)))

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

