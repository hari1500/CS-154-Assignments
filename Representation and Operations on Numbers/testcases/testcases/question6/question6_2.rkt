#lang racket
(require "q6.rkt")
(equal? (s-mult 4 '(1 1 1 1) '(1 1 1 1))
        '(0 0 0 0 0 0 0 1))
