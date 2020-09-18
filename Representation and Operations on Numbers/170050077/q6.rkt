#lang racket
(define (s-mult n a b)
  (define 2n (* 2 n))
  (truncate 2n (u-mult 2n (n2m n 2n a) (n2m n 2n b))))

(define (u-mult-1 n a p)
  (if (= p 1) a (generate-0 n)))
(define (generate-0 n)
    (if (= n 1) (cons 0 null) (cons 0 (generate-0 (- n 1)))))

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

(define (n2m n m a)
  (if(= n m) a
     (n2m (+ n 1) m (cons (car a) a))))

(define (u-mult n a b)
  (define (shift-n-n2m n m a)
    (if(null? a) '()
       (append (list (n2mu n m (car a))) (shift-n-n2m (+ n 1) m (map shift-l (cdr a))))))
  (define (u-mult-hlpr n a b )
    (if (null? b) null
        (append (u-mult-hlpr n a (cdr b)) (list(u-mult-1 n a (car b))))))
  (define (f a b) (u-add (* 2 n) a b))
  (foldr f (generate-0 (* 2 n)) (shift-n-n2m n (* 2 n)(u-mult-hlpr n a b))))