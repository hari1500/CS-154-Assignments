#lang racket
(define (b2u n a)
  (define len (length a))
  (define (b2u-hlpr a exp val)
    (cond[(= exp 0) (+ val (car a))]
         [(= (car a) 0) (b2u-hlpr (cdr a) (- exp 1) (* 2 val))]
         [else(b2u-hlpr (cdr a) (- exp 1) (* 2 (+ val 1)))]))
  (if(<= len n)
     (b2u-hlpr a (- len 1) 0)
     (begin (display "There are only ")
            (display n)
            (display " bits"))))