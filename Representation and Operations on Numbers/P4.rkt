#lang racket
;;;;;;;;;some usefull functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (single-add a b c)
  (cond[(= 0 (+ a b c)) (list 0 0)]
       [(= 1 (+ a b c)) (list 0 1)]
       [(= 2 (+ a b c)) (list 1 0)]
       [else(list 1 1)]))

(define (truncate n x)
  (if (= (length x) n) x
      (truncate n (cdr x))))

(define (greater-or-equal? a b)
  (if (null? a) #t
      (cond[(> (car a) (car b)) #t]
           [(= (car a) (car b)) (greater-or-equal? (cdr a) (cdr b))]
           [else #f])))

(define (2scompliment n x)
  (define (alter x)
    (if(= x 1) 0 1))
  (define (1scompliment x)
    (map (lambda(ele) (alter ele)) x))
  (truncate n (u-add n (1scompliment x) (generate-1 n))))

(define (generate-1 n)
    (if (= n 1) (cons 1 null) (cons 0 (generate-1 (- n 1)))))

(define (generate-0 n)
    (if (= n 1) (cons 0 null) (cons 0 (generate-0 (- n 1)))))
;;;;;;;;;;;;;addition of unsigned ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (u-add n a b)
  (define a1 (reverse a) )
  (define b1 (reverse b))
  (define (u-add-rev-hlpr n a b sum)
    (cond[(null? a) sum]
         [(null? (cdr a)) (append (single-add (car a) (car b) (car sum)) (cdr sum))]     
         [else (u-add-rev-hlpr (- n 1) (cdr a) (cdr b)
                               (append (single-add (car a) (car b) (car sum)) (cdr sum)))]))
  (truncate n (u-add-rev-hlpr (- n 1) (cdr a1) (cdr b1) (single-add (car a1) (car b1) 0))))
;;;;;;;;;;;;;subtraction of unsigned;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (u-sub n a b)
  (if (not (greater-or-equal? a b)) (generate-0 n) 
      (u-add n a (2scompliment n b))))