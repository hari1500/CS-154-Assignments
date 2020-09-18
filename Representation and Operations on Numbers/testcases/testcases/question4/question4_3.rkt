#lang racket
(require "q4.rkt")
(equal? (u-add 8 '(0 1 0 0 1 1 0 1) '(1 1 0 1 0 1 1 1)) 
        '(0 0 1 0 0 1 0 0))
