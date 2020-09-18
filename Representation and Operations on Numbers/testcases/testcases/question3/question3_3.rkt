#lang racket
(require "q3.rkt")
(equal? (n2m 6 16 '(1 1 0 1 1 0)) 
'(1 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0))
