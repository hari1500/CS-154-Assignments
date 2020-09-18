#lang racket
;;;;;;;;;binary to unsigned;;;;;;;;;;;;;;;;;;;;;;;;;;
;;assuming all elements of list a are 0's and 1's.;;;
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
;;;;;;;;;unsigned to binary;;;;;;;;;;;;;;;;;;;;;;;;;
(define (u2b n x)
  (define (u2b-hlpr n x a)
    (cond[(and (= n 1)(= 0 (quotient x 2))) (cons (modulo x 2) a)]
         [(and (= n 1)(not (= 0 (quotient x 2))))
          (display "Given number is out of range")]
         [else(u2b-hlpr (- n 1) (quotient x 2) (cons (modulo x 2) a))]))
  (u2b-hlpr n x '()))