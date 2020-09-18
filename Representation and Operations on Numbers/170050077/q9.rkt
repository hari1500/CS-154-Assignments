#lang racket
(define (u2b n x)
  (define (u2b-hlpr n x a)
    (cond[(and (= n 1)(= 0 (quotient x 2))) (cons (modulo x 2) a)]
         [(= n 1) (display "Given number is out of range")]
         [else(u2b-hlpr (- n 1) (quotient x 2) (cons (modulo x 2) a))]))
  (u2b-hlpr n x '()))

(define (b2u n a)
  (define (b2u-hlpr a exp val)
    (cond[(= exp 0) (+ val (car a))]
         [(= (car a) 0) (b2u-hlpr (cdr a) (- exp 1) (* 2 val))]
         [else(b2u-hlpr (cdr a) (- exp 1) (* 2 (+ val 1)))]))
  (b2u-hlpr a (- n 1) 0))

(define (fix2d n a)
  (define (deci-hlpr a val)
    (if (null? a) (/ val 2) (deci-hlpr (cdr a) (+ (car a) (/ val 2)))))
  (let* ([a/2 (divide-2 n a)])
    (+ (b2u n (car a/2)) (deci-hlpr (reverse (cdr a/2)) 0))))

(define (divide-2 n a)
  (define (divide-2-hlpr  a b)
    (if (= n (length a)) b
        (divide-2-hlpr (cdr a) (cons (append (car b) (list (car a))) (cdr a)))))
  (divide-2-hlpr a (cons null null)))

(define (d2fix n a)
    (define (d2b n a)
    (if (= 0 n) '() (cons (floor (* 2 a)) (d2b (- n 1) (- (* 2 a) (floor (* 2 a)))))))
  (append (map inexact->exact (u2b n (floor a)))
          (map inexact->exact (d2b n (- a (floor a))))))