#lang racket
(require "q11.rkt")
(equal? (divide 5 4 '(0 (1 0 1 0 1) (0 0 1 1)) '(0 (1 1 0 0 0) (1 0 1 1))) '(0 (0 1 0 1 1) (0 1 1 0)))