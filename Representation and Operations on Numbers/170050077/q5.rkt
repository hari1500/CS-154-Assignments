#lang racket
(define (u-mult-1 n a p)
  (if (= p 1) a (make-list n 0)))

(define (shift-l a)
  (append a (list 0)))

(define (single-add a b c)
  (cond[(= 0 (+ a b c)) (list 0 0)]
       [(= 1 (+ a b c)) (list 0 1)]
       [(= 2 (+ a b c)) (list 1 0)]
       [else(list 1 1)]))

(define (truncate n x)
  (if (= (length x) n) x
      (truncate n (cdr x))))

(define (u-add n a b)
  (define a1 (reverse a) )
  (define b1 (reverse b))
  (define (u-add-rev-hlpr n a b sum)
    (cond[(null? a) sum]
         [(null? (cdr a)) (append (single-add (car a) (car b) (car sum)) (cdr sum))]     
         [else (u-add-rev-hlpr (- n 1) (cdr a) (cdr b)
                               (append (single-add (car a) (car b) (car sum)) (cdr sum)))]))
  (truncate n (u-add-rev-hlpr (- n 1) (cdr a1) (cdr b1) (single-add (car a1) (car b1) 0))))

(define (n2mu n m a)
  (if(= n m) a
     (n2mu (+ n 1) m (cons 0 a))))

(define (u-mult n a b)
  (define (shift-n-n2m n m a)
    (if(null? a) '()
       (append (list (n2mu n m (car a))) (shift-n-n2m (+ n 1) m (map shift-l (cdr a))))))
  (define (u-mult-hlpr n a b )
    (if (null? b) null
        (append (u-mult-hlpr n a (cdr b)) (list(u-mult-1 n a (car b))))))
  (define (f a b) (u-add (* 2 n) a b))
  (foldr f (make-list (* 2 n) 0) (shift-n-n2m n (* 2 n)(u-mult-hlpr n a b))))