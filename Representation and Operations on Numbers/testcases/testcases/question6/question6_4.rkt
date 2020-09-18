#lang racket
(require "q6.rkt")
(equal? (s-mult 6 '(1 0 1 1 0 1) '(0 0 0 0 0 1))
        '(1 1 1 1 1 1 1 0 1 1 0 1))
