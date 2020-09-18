#lang racket
(define (n2m n m a)
  (if(= n m) a
     (n2m (+ n 1) m (cons (car a) a))))