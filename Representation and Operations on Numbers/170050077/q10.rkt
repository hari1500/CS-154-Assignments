#lang racket
(define (float2d k l a)
  (define bias (- (expt 2 (- k 1)) 1))
  (cond[(and (equal? (make-list k 0) (cadr a)) (= 0 (car a)))
        (* (/ (b2u l (cddr a)) (expt 2 l)) (expt 2 (- 1 bias)))]
       [(and (equal? (make-list k 0) (cadr a)) (= 1 (car a)))
        (- (* (/ (b2u l (cddr a)) (expt 2 l)) (expt 2 (- 1 bias))))]
       [(and (equal? (make-list k 1) (cadr a)) (= 0 (car a))
             (equal? (make-list l 0) (cddr a))) (display "Plus Infinity")]
       [(and (equal? (make-list k 1) (cadr a)) (= 1 (car a))
             (equal? (make-list l 0) (cddr a))) (display "Minus Infinity")]
       [(equal? (make-list k 1) (cadr a)) (display "NaN")]
       [(= 0 (car a)) (* (+ 1 (/ (b2u l (cddr a)) (expt 2 l)))
                         (expt 2 (- (b2u k (cadr a)) bias)))]
       [else(- (* (+ 1 (/ (b2u l (cddr a)) (expt 2 l)))
                  (expt 2 (- (b2u k (cadr a)) bias))))]))

(define (d2float k l a)
  (define (d2float-helper p val)
    (cond[(and (>= val 1) (<= val (- 2 (/ 1 (expt 2 l))))) (cons (+ 0 p) (- val 1))]
         [(< val 1) (d2float-helper (- p 1) (* val 2))]
         [else (d2float-helper (+ p 1) (/ val 2))]))  
  (define (maxval k l)
    (let*[(maxexp (append (make-list (- k 1) 1) (list 0)))
          (maxfrac (make-list l 1))]
      (float2d k l (append (list 0) (list maxexp) maxfrac))))
  (define bias (- (expt 2 (- k 1)) 1))  
   (cond [(and (< a (/ 1 (expt 2 bias))) (>= a 0))
          (append (list 0) (list (make-list k 0))
                  (map inexact->exact (d2b l (* a (expt 2 (- bias 1))))))]
         [(and (< (- a) (/ 1 (expt 2 bias))) (< a 0))
          (append (list 1) (list (make-list k 0))
                  (map inexact->exact (d2b l (* (- a) (expt 2 (- bias 1))))))]
         [(> a (maxval k l)) (append (list 0) (list (append (make-list (- k 1) 1) (list 0))) (make-list l 1))]
         [(> (- a) (maxval k l)) (append (list 1) (list(append (make-list (- k 1) 1) (list 0))) (make-list l 1))]
         [(> a 0) (let*[(temp (d2float-helper 0 a))]
                    (append (list 0) (list (u2b k (+ bias (car temp))))
                            (map inexact->exact (u2b l (floor (* (cdr temp) (expt 2 l)))))))]
         [(< a 0) (let*[(temp (d2float-helper 0 (- a)))]
                    (append (list 1) (list (u2b k (+ bias (car temp))))
                            (map inexact->exact (u2b l (floor (* (cdr temp) (expt 2 l)))))))]))     
(define (d2b n a)
    (if (= 0 n) '() (cons (floor (* 2 a)) (d2b (- n 1) (- (* 2 a) (floor (* 2 a)))))))
(define (b2u n a)
  (define (b2u-hlpr a exp val)
    (cond[(= exp 0) (+ val (car a))]
         [(= (car a) 0) (b2u-hlpr (cdr a) (- exp 1) (* 2 val))]
         [else(b2u-hlpr (cdr a) (- exp 1) (* 2 (+ val 1)))]))
  (b2u-hlpr a (- n 1) 0))
(define (u2b n x)
  (define (u2b-hlpr n x a)
    (cond[(and (= n 1)(= 0 (quotient x 2))) (cons (modulo x 2) a)]
         [(= n 1) (display "Given number is out of range")]
         [else(u2b-hlpr (- n 1) (quotient x 2) (cons (modulo x 2) a))]))
  (u2b-hlpr n x '()))