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
  (if (not (greater-or-equal? a b)) (make-list n 0)
      (u-add n a (2scompliment n b))))

(define (u-mult-1 n a p)
  (if (= p 1) a (make-list n 0)))

(define (slice l a b)
  (cond[(and (= a 1) (= b 1)) (cons (car l) null)]
       [(and (= a 1) (> b 1)) (cons (car l) (slice (cdr l) a (- b 1)))]
       [else (slice (cdr l) (- a 1) (- b 1))]))

(define (shift-l a)
  (append a (list 0)))

(define (n2mu n m a)
  (if(= n m) a
     (n2mu (+ n 1) m (cons 0 a))))

(define (u-div n x y)
  (define (u-div-hlpr n x y res)
    (if (greater-or-equal? x y) (u-div-hlpr n (u-sub n x y) y
                                            (list (u-add n (car res) (generate-1 n)) (cdr res)))
        (list (car res) x)))
  (u-div-hlpr n x y (cons (make-list n 0) (make-list n 0))))

(define (u-mult n a b)
  (define (shift-n-n2m n m a)
    (if(null? a) '()
       (append (list (n2mu n m (car a))) (shift-n-n2m (+ n 1) m (map shift-l (cdr a))))))
  (define (u-mult-hlpr n a b )
    (if (null? b) null
        (append (u-mult-hlpr n a (cdr b)) (list(u-mult-1 n a (car b))))))
  (define (f a b) (u-add (* 2 n) a b))
  (foldr f (make-list (* 2 n) 0) (shift-n-n2m n (* 2 n)(u-mult-hlpr n a b))))



(define (mult k l a b)
  (define zero (list 0 (make-list k 0) (make-list l 0)))
  (define bias (- (expt 2 (- k 1)) 1))
  (define sum-exp (u-sub (+ k 1) (u-add (+ k 1) (append (list 0) (cadr a)) (append (list 0)(cadr b))) (u2b (+ k 1) bias)))
  (define (exp-overflow? lst) (if (= 1 (car lst)) #t #f))
  (define (maxval k l)
    (let*[(maxexp (append (make-list (- k 1) 1) (list 0)))
          (maxfrac (make-list l 1))]
      (float2d k l (append (list 0) (list maxexp) maxfrac))))
  (define (significand a)
    (if (equal? (cadr a) (make-list k 0)) (append (list 0) (cddr a)) (append (list 1) (cddr a))))
  (define (normalize a)
    (cond [(equal? (make-list k 0) (cadr a)) (if(= 0 (car (cddr a))) (append (list (car a)) (list(cadr a)) (slice (cddr a) 2 (+ l 1)))
                                                (append (list (car a)) (list (u-add (length (cadr a)) (cadr a) (generate-1 (length (cadr a)))))
                                                      (slice (cddr a) 2 (+ l 1))))]
          [(and (= 1 (car (cddr a))) (equal? (cadr a) (append (make-list (- l 1) 1) (list 0)))) (maxval k l)]
          [(= 1 (car (cddr a))) (append (list (car a)) (list(u-add (length (cadr a)) (cadr a) (generate-1 (length (cadr a)))))  (slice (cddr a) 2 (+ l 1)))]
          [(= 0 (car (cddr a))) (append (list (car a)) (list (cadr a)) (slice (cddr a) 3 (+ l 2)))]))
  (define (normalize1 lst p)
    (cond [(null? (cadr lst)) (list lst p)]
          [(= 1 (cadr lst)) (list (slice lst 3 (+ l 2)) p)]
          [else (normalize1 (cdr lst) (+ p 1))]))
  (cond[(or (equal? a zero) (equal? b zero)) zero]
       [(exp-overflow? sum-exp) (if (or (and (= 0 (car a)) (= 0 (car b)))
                                        (and (= 1 (car a)) (= 1 (car b))))
                                    (maxval k l)
                                    (append (list 1) (cdr (maxval k l))))]
       [(and (equal? sum-exp (make-list (+ k 1) 0)) (not(equal? (u-add (+ k 1) (append (list 0) (cadr a)) (append (list 0)(cadr b)))
                                                                (u2b (+ k 1) bias)))) zero]
       [else (if (and (not (equal? (cadr a) (make-list k 0))) (not(equal? (cadr b) (make-list k 0))))
                      (normalize (append (list (if (or (and (= 0 (car a)) (= 0 (car b))) (and (= 1 (car a)) (= 1 (car b)))) 0 1))
                                       (list (cdr sum-exp)) (u-mult (+ l 1) (significand a) (significand b))))
                      (let*[(temp (normalize1 (u-mult (+ l 1) (significand a) (significand b)) -1))]
                        (if (< (b2u (+ k 1) sum-exp) (cadr temp)) zero
                            (append (list (if (or (and (= 0 (car a)) (= 0 (car b))) (and (= 1 (car a)) (= 1 (car b)))) 0 1))
                                  (list (u-sub k (cdr sum-exp) (u2b k (cadr temp)))) (car temp)))))]))
  
(define (divide k l a b)
  (define zero (append (list 0) (list (make-list k 0)) (make-list l 0)))
  (define bias (- (expt 2 (- k 1)) 1))
  (define diff-exp (u-add (+ k 1) (append (list 0)(u-sub k (cadr a) (cadr b))) (u2b (+ k 1) bias)))
  (define (exp-overflow? lst) (if (= 1 (car lst)) #t #f))
  (define (maxval k l)
    (let*[(maxexp (append (make-list (- k 1) 1) (list 0)))
          (maxfrac (make-list l 1))]
      (append (list 0) (list maxexp) maxfrac)))
  (define (significand a)
    (if (equal? (cadr a) (make-list k 0)) (append (list 0) (cddr a)) (append (list 1) (cddr a))))
  (define (normalize a)
    (cond [(equal? (make-list k 0) (cadr a)) (if(= 0 (car (cddr a))) (append (list(car a)) (list (cadr a)) (slice (cddr a) 2 (+ l 1)))
                                                (append (list (car a)) (list (u-add (length (cadr a)) (cadr a) (generate-1 (length (cadr a)))))
                                                      (slice (cddr a) 2 (+ l 1))))]
          [(and (= 1 (car (cddr a))) (equal? (cadr a) (append (make-list (- l 1) 1) (list 0)))) (maxval k l)]
          [(= 1 (car (cddr a))) (append (list (car a)) (list(u-add (length (cadr a)) (cadr a) (generate-1 (length (cadr a)))))  (slice (caddr a) 2 (+ l 1)))]
          [(= 0 (car (cddr a))) (append (list (car a)) (list (cadr a)) (cdddr a))]))
  (define (normalize1 lst p)
    (cond [(null? (cdr lst)) (list lst p)]
          [(= 1 (cadr lst)) (list (slice lst 3 (+ l 2)) p)]
          [else (normalize1 (cdr lst) (+ p 1))]))
  (cond[(equal? b zero) (if (= 0 (car b)) (display "Plus Infinity") (display "Minus Infinity"))]
       [(equal? a zero) zero]
       [(exp-overflow? diff-exp) (if (or (and (= 0 (car a)) (= 0 (car b)))
                                        (and (= 1 (car a)) (= 1 (car b))))
                                    (maxval k l)
                                    (append (list 1) (cdr (maxval k l))))]
       [(and (equal? diff-exp (make-list (+ k 1) 0)) (not(equal? (u-sub (+ k 1) (append (list 0) (cadr a)) (append (list 0)(cadr b)))
                                                                (u2b (+ k 1) bias)))) zero]
       [else (if (and (not (equal? (cadr a) (make-list k 0))) (not(equal? (cadr b) (make-list k 0))))
                      (normalize (append (list (if (or (and (= 0 (car a)) (= 0 (car b))) (and (= 1 (car a)) (= 1 (car b)))) 0 1))
                                       (list (cdr diff-exp)) (car (u-div (+ l 1) (significand a) (significand b)))))
                      (let*[(temp (normalize1 (car (u-div (+ l 1) (significand a) (significand b))) -1))]
                        (if (< (b2u (+ k 1) diff-exp) (cadr temp)) zero
                            (append (list (if (or (and (= 0 (car a)) (= 0 (car b))) (and (= 1 (car a)) (= 1 (car b)))) 0 1))
                                  (list (u-sub k (cdr diff-exp) (u2b k (cadr temp)))) (car temp)))))]))