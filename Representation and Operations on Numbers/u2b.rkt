#lang racket
(define (u2b n x)
  (define (u2b-hlpr n x a)
    (cond[(and (= n 1)(= 0 (quotient x 2))) (cons (modulo x 2) a)]
         [(and (= n 1)(not (= 0 (quotient x 2))))
          (display "Given number is out of range")]
         [else(u2b-hlpr (- n 1) (quotient x 2) (cons (modulo x 2) a))]))
  (u2b-hlpr n x '()))