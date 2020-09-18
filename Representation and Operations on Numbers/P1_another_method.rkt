#lang racket
;;;;;;;;;;;;defining a power function;;;;;;;;;;;;;;;
(define (pow a x)
  (define (pow-helper n value)
    (cond[(= n 0) value]
         [else(pow-helper (- n 1) (* value a))]))
  (pow-helper x 1))
;;;;;;;;;binary to unsigned;;;;;;;;;;;;;;;;;;;;;;;;;;
;;assuming all elements of list a are 0's and 1's.;;;
(define (b2u n a)
  (define len (length a))
  (define (b2u-hlpr a exp val)
    (cond[(= exp 0) (+ val (car a))]
         [else(b2u-hlpr (cdr a) (- exp 1) (+ val (* (car a) (pow 2 exp)
                                                    )))]))
  (if(<= len n)
     (b2u-hlpr a (- len 1) 0)
     (begin (display "There are only ")
            (display n)
            (display " bits"))))
;;;;;;;;;unsigned to binary;;;;;;;;;;;;;;;;;;;;;;;;;
(define (u2b n x)
  (define (u2b-hlpr n x a)
    (cond[(= n 1) (append (list (modulo x 2)) a)]
         [else(u2b-hlpr (- n 1) (quotient x 2) (append
                                        (list (remainder x 2)) a))]))
  (if(and(<= 0 x) (< x (pow 2 n)))
     (u2b-hlpr n x '())
     (begin (display "Given number is out of range."))))