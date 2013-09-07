#lang racket
(require racket/trace)
(define (iperm s a)
  (if (null? s)
      (list a)
  (append-map(λ(x) iperm(remove x s) (cons x a)) s)))
;;(trace iperm)
;;(iperm '(1 2 3 4 5) '())

(define (permutations s)
  (if (null? s)
      ; empty set?
      (list '())
      ; sequence containing empty set
      (append-map (λ (x)
                 (map (λ (p) (cons x p))
                      (permutations (remove x s))))
               s)))


(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))
;;(trace enumerate-interval)
(enumerate-interval 5 25)
(permutations (enumerate-interval 1 4))