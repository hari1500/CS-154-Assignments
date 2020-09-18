#lang racket
(require "q4.rkt")
(equal? (u-add 6 '(1 1 1 1 1 1) '(1 1 1 1 1 1)) 
        '(1 1 1 1 1 0))
