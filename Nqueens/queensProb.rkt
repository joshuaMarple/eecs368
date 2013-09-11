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

(define (diagHelper? new check g)
  (if (or (= (+ new g) check) (= (- new g) check))
      #t
      #f))

(define (diag? n l)
  (ormap (λ(x y) (diagHelper? n x y)) l (enumerate-interval 1 (length l))))

(define (q n)
  (displayln 
   (length (iqueens (enumerate-interval 1 n) '()))))

(define (test t)
  (for-each q (enumerate-interval 1 t)))

(test 20)