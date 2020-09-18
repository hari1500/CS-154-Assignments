#lang racket
(require "q11.rkt")
(equal?  (mult 5 4 '(1 (1 0 0 0 0) (1 0 0 0)) '(0 (1 0 0 1 0) (1 1 1 0)))  '(1 (1 0 1 0 0) (0 1 1 0)))