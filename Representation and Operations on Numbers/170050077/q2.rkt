#lang racket
;;;;;;;;;binary to signed;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;assuming all elements of list a are 0's and 1's.;;;
(define (b2s n a)
  (define (b2u-hlpr a exp val)
    (cond[(= exp 0) (+ val (car a))]
         [(= (car a) 0) (b2u-hlpr (cdr a) (- exp 1) (* 2 val))]
         [else(b2u-hlpr (cdr a) (- exp 1) (* 2 (+ val 1)))]))
     (b2u-hlpr (cdr a) (- n 2) (* (- 2) (car a))))
;;;;;;;;;;signed to binary;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (s2b n x)
  (define (s2b-hlpr1 n x a)
    (cond[(and (= n 1)(= 0 (quotient x 2))) (cons 0 (cons (modulo x 2) a))]
         [(and (= n 1)(not (= 0 (quotient x 2))))
          (display "Given number is out of range")]
         [else(s2b-hlpr1 (- n 1) (quotient x 2) (cons (modulo x 2) a))]))
  (define (s2b-hlpr2 n x a)
    (cond[(and (= n 1)(= 0 (quotient x 2))) (cons 1 (cons (modulo (+ x 1) 2) a))]
         [(and (= n 1)(not (= 0 (quotient (+ x 1) 2))))
          (display "Given number is out of range")]
         [else(s2b-hlpr2 (- n 1) (quotient x 2) (cons (modulo (+ x 1) 2) a))]))

  (if(>= x 0)
     (s2b-hlpr1 (- n 1) x '())
     (s2b-hlpr2 (- n 1) (- (+ x 1)) '())))