#lang racket
;;bits of x is more than n
(define (truncate n x)
  (if (= (length x) n) x
      (truncate n (cdr x))))