#lang racket
(define (queens board-size)
 (let loop ((k 1) (pp-pair (cons '() (enumerate-interval 1 board-size))))
   (let ((position (car pp-pair))
         (potential-rows (cdr pp-pair)))
    (if (> k board-size) 
        (list position)
        (append-map (lambda (pp-pair) (loop (++ k) pp-pair)) 
         (filter (lambda (pp-pair) (safe? k (car pp-pair))) ;keep only safe
          (map (lambda (new-row) 
                (cons (adjoin-position new-row k position) 
                      (remove-row new-row potential-rows))) ;make pp-pair
           potential-rows))))))) 
(define (++ k) (+ k 1))
;auxiliary functions not listed 