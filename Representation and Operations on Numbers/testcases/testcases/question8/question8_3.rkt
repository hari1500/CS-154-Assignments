#lang racket
(require "q8.rkt")
(or 
(equal? (s-div 8 '(1 0 1 0 0 0 0 1) '(1 1 1 0 1 1 0 1))
'((0 0 0 0 0 1 0 1) 0 0 0 0 0 0 0 0))
(equal? (s-div 8 '(1 0 1 0 0 0 0 1) '(1 1 1 0 1 1 0 1))
'((0 0 0 0 0 1 0 1) (0 0 0 0 0 0 0 0))))
