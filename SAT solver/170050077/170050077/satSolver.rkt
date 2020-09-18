#lang racket

(require "utilities.rkt")

(define assign #hash())

; Fill in your code here. Should finally define a function
; called dpll which returns true or false. Should additionally
; store the satisfying assignment in the variable assign.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;list-from-exp
(define (list-from-exp t l)
  (cond [(Var? t) (append (list (list (Var-lit t))) l)]
        [(Not? t) (append (list (list (- (Var-lit (Not-e t))))) l)]
        [(And? t) (car (append (list (append (list-from-exp (And-x t) null) (list-from-exp (And-y t) null))) l))]
        [(Or? t) (append (list (append (car (list-from-exp (Or-x t) null)) (car (list-from-exp (Or-y t) null)))) l)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;number list-of-clauses -> list-of-clauses-removing-unit-clause
(define (unitProp-h unit-clause lst)
    (cond [(null? lst) null]
          [(search unit-clause (car lst)) (unitProp-h unit-clause (cdr lst))]
          [(search (- unit-clause) (car lst)) (append (list (remove (- unit-clause) (car lst)))(unitProp-h unit-clause (cdr lst)))]
          [else (append (list (car lst)) (unitProp-h unit-clause (cdr lst)))]))
;;collects-first-unit-clauses from list-of-clauses
(define (collect-unit-clause lst)
  (cond [(null? lst) null]
        [(equal? 1 (length (car lst))) (car lst)]
        [else (collect-unit-clause (cdr lst))]))
;;searches for a element in a list;;no bugs
(define (search ele lst)
  (cond [(null? lst) #f]
        [(equal? ele (car lst)) #t]
        [else (search ele (cdr lst))]))
;;samePolarity
(define (samePolarity lst)
  (define temp (append* lst))
  (define (samePolarity-hlpr l)
    (cond [(null? l) null]
          [(and (not (search (- (car l)) temp)) (search (car l) (remove (car l) temp)))
           (list (car l))]
          [else (samePolarity-hlpr (cdr l))]))
  (samePolarity-hlpr temp))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;for assigning the unit-clauses into "assign";;no bugs  
(define (assigning u-clauses)
  (define (assigning-hlpr num)
    (cond[(< 0 num) (set! assign (dict-set assign num #t))]
         [else (set! assign (dict-set assign (- num) #f))]))
  (cond [(null? u-clauses) (set! u-clauses null)]
        [else (begin (assigning-hlpr (car u-clauses)) (assigning (cdr u-clauses)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;dpll
;;;;formula->boolean
(define (dpll Exp)
  (define lst (list-from-exp Exp null))
  (define (dpll-hlpr l)
    (define unit-clause (collect-unit-clause l))
    (define =polarity (samePolarity l))
    (define (literal-elim lst)
      (cond [(null? =polarity) lst]
            [else (foldr unitProp-h lst =polarity)]))
    (define (unitProp lst)
      (cond [(null? unit-clause) lst]
            [else (unitProp-h (car unit-clause) lst)]))
    (cond [(null? l) #t]
          [(search null l) #f]
          [(not(null? unit-clause)) (if (dpll-hlpr (unitProp l)) (begin (assigning unit-clause) #t) #f)]
          [(not(null? =polarity)) (if (dpll-hlpr (literal-elim l)) (begin (assigning =polarity) #t) #f)]
          [else (let*([temp (unitProp-h (caar l) l)]
                      [temp1 (unitProp-h (- (caar l)) l)])
                  (cond [(dpll-hlpr temp) (begin (assigning (list (caar l))) #t)]
                        [(dpll-hlpr temp1) (begin (assigning (list (- (caar l)))) #t)]
                        [else #f]))]))
  (set! assign #hash())
  (dpll-hlpr lst))