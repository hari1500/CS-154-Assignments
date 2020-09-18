#lang racket
;;(require parser-tools/lex
;;         parser-tools/yacc)
(require "declarations.rkt")
;;(provide (all-defined-out))
(require "utilities.rkt")

(define t1 (maketree "(a|b)*bba|cc*"))
(define t2 (maketree "((abcdef)*|(@|d)*)*"))

(define (union s1 s2)
  (cond[(null? s1) s2]
       [else(if(search-in-list (car s1) s2)
               (union (cdr s1) s2)
               (union (cdr s1)
               (sort(append s2 (list (car s1))) <)))]))

(define (union-with-out-sort s1 s2)
  (cond [(null? s1) s2]
        [else (if (search-in-list (car s1) s2)
                  (union-with-out-sort (cdr s1) s2)
                  (union-with-out-sort (cdr s1)
                                       (append s2 (list (car s1)))))]))

(define (corres-tree-hlpr n t)
  (cond[(Epsilon? t) (if (= n (Epsilon-n t)) (list t) null)]
       [(Literal? t) (if (= n (Literal-n t)) (list t) null)]
       [(Or? t) (if (= n (Or-n t)) (list t)
                    (list (corres-tree-hlpr n (Or-t1 t))
                          (corres-tree-hlpr n (Or-t2 t))))]
       [(Then? t) (if (= n (Then-n t)) (list t)
                      (list (corres-tree-hlpr n (Then-t1 t))
                            (corres-tree-hlpr n (Then-t2 t))))]
       [(Star? t) (if (= n (Star-n t)) (list t)
                      (list (corres-tree-hlpr n (Star-t t))))]))
(define (flatten lst)
  (cond[(null? lst) lst]
       [(list? (car lst)) (append (flatten (car lst))
                                  (flatten(cdr lst)))]
       [else (append (list (car lst)) (flatten (cdr lst)))]))
(define (corres-tree n t)
  (car (flatten(corres-tree-hlpr n t))))

(define (numbertill n)
  (cond[(= n 1) (list 1)]
       [else (append (numbertill (- n 1)) (list n))]))

(define (buildNullable-hlpr t)
  (cond[(Epsilon? t) #t]
       [(Literal? t) #f]
       [(Or? t) (or (buildNullable-hlpr (Or-t1 t))
                    (buildNullable-hlpr (Or-t2 t)))]
       [(Then? t) (and (buildNullable-hlpr (Then-t1 t))
                       (buildNullable-hlpr (Then-t2 t)))]
       [(Star? t) #t]))

(define (buildNullable t)
  (define (fun m)
    (let* ([temp (corres-tree m t)])
      (cons m (buildNullable-hlpr temp))))
  (map fun (numbertill (getNodeNumber t))))

(define (buildFirst-hlpr t)
  (cond[(Epsilon? t) null]
       [(Literal? t) (list (Literal-n t))]
       [(Or? t) (union (buildFirst-hlpr (Or-t1 t))
                       (buildFirst-hlpr (Or-t2 t)))]
       [(Then? t) (if (buildNullable-hlpr (Then-t1 t))
                      (union (buildFirst-hlpr (Then-t1 t))
                             (buildFirst-hlpr (Then-t2 t)))
                      (buildFirst-hlpr (Then-t1 t)))]
       [(Star? t) (buildFirst-hlpr (Star-t t))]))

(define (buildFirst t)
  (define (fun m)
    (append (list m) (buildFirst-hlpr (corres-tree m t))))
  (map fun (numbertill (getNodeNumber t))))

(define (buildLast-hlpr t)
  (cond[(Epsilon? t) null]
       [(Literal? t) (list (Literal-n t))]
       [(Or? t) (union (buildLast-hlpr (Or-t1 t))
                       (buildLast-hlpr (Or-t2 t)))]
       [(Then? t) (if (buildNullable-hlpr (Then-t2 t))
                      (union (buildLast-hlpr (Then-t1 t))
                             (buildLast-hlpr (Then-t2 t)))
                      (buildLast-hlpr (Then-t2 t)))]
       [(Star? t) (buildLast-hlpr (Star-t t))]))

(define (buildLast t)
  (define (fun m)
    (append (list m) (buildLast-hlpr (corres-tree m t))))
  (map fun (numbertill (getNodeNumber t))))

(define (buildFollow-hlpr i t)
  (define (buildFollow-hlpr1 index)
    (cond[(> index (getNodeNumber t)) null]
         [else (let* ([t1 (corres-tree index t)])
          ( cond [(Then? t1) (cond[(and (buildNullable-hlpr (Then-t2 t1))
                                        (search-in-list i (buildLast-hlpr (Then-t1 t1))))
                                   (union (buildFirst-hlpr (Then-t2 t1))
                                          (buildFollow-hlpr1 (+ 1 index)))]
                                  [(search-in-list i (buildLast-hlpr (Then-t1 t1)))
                                   (buildFirst-hlpr (Then-t2 t1))]
                                  [else (buildFollow-hlpr1 (+ index 1))])]
                 [(Star? t1) (if (search-in-list i (buildLast-hlpr (Star-t t1)))
                                 (union (buildFirst-hlpr (Star-t t1))
                                        (buildFollow-hlpr1 (+ index 1)))
                                 (buildFollow-hlpr1 (+ index 1)))]
                 [else (buildFollow-hlpr1 (+ index 1))]))]))
  (let* ([check (corres-tree i t)])
    (cond[(or (Then? check) (Or? check)
              (Star? check)) null]
         [else (buildFollow-hlpr1 i)])))

(define (buildFollow t)
  (define (fun m y)
    (if (null? (buildFollow-hlpr m t)) y
        (append (list (append (list m) (buildFollow-hlpr m t))) y)))
  (foldr fun null (numbertill (getNodeNumber t))))

(define (buildGraph regexp)
  (let* ([t (maketree regexp)])
    (Graph (buildFirst-hlpr t)
           (nodes (list (buildFirst-hlpr t)) t)
           (trans t)
           (rednodes t)
           (symbols t))))

(define (symbols t)
  (cond [(Epsilon? t) null]
        [(Literal? t) (list (Literal-c t))]
        [(Or? t) (union-with-out-sort (symbols (Or-t1 t))
                                      (symbols (Or-t2 t)))]
        [(Then? t) (union-with-out-sort (symbols (Then-t1 t))
                                        (symbols (Then-t2 t)))]
        [(Star? t) (symbols (Star-t t))]))


(define (corres-sym i t)
  (Literal-c (corres-tree i t)))

;(define (nodes l t)
 ; (define sym (remove "#" (symbols t)))
  ;(define (sub-nodes p)
   ; (define (g s)
    ;  (define (h x y)
     ;   (if (equal? (corres-sym x t) s) (union (buildFollow-hlpr x t) y) y))
      ;(foldr h null p))
    ;(map g sym))
;  (define (check l)
 ;   (define (dot x y) (and x y))
  ;  (define (f ele)
   ;   (if (subset? (sub-nodes ele) l) #t #f))
    ;(foldr dot #t (map f l)))
  ;(if (check l) (remove null (set->list (list->set l)))
   ;   (nodes (append (append* (map sub-nodes l)) l) t)))

(define (nodes l t)
  (define sym (remove "#" (symbols t)))
  (define (sub-nodes p)
    (define (g s)
      (define (h x y)
        (if (equal? (corres-sym x t) s) (union (buildFollow-hlpr x t) y) y))
      (foldr h null p))
    (set->list (list->set (map g sym))))
  (let*([temp (append (append* (map sub-nodes l)) l)])
    (if (equal? (set->list (list->set temp)) l) (remove null l)
        (nodes (set->list (list->set temp)) t))))

(define (subset? s1 s2)
  (cond [(null? s1) #t]
        [(search-in-list (car s1) s2) (subset? (cdr s1) s2)]
        [else #f]))

(define (rednodes t)
  (define allnodes (nodes (list (buildFirst-hlpr t)) t))
  (define get#number (- (getNodeNumber t) 1))
  (define (rednodeshlpr l)
    (cond [(null? l) null] 
          [(search-in-list get#number (car l)) (append (list (car l)) (rednodeshlpr (cdr l)))]
          [else (rednodeshlpr (cdr l))]))
  (rednodeshlpr allnodes))

(define (trans t)
  (define allnodes (nodes (list (buildFirst-hlpr t)) t))
  (define sym (remove "#" (symbols t)))
  (define (fun p)
    (define (g s)
      (define (h x y)
        (if (equal? (corres-sym x t) s) (union (buildFollow-hlpr x t) y) y))
      (let* ([consecutive (foldr h null p)])
        (if (null? consecutive ) null (Trans p s (foldr h null p)))))
    (map g sym))
  (remove null (set->list (list->set (append* (map fun allnodes))))))
