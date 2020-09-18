#lang racket
(require "q5.rkt")
(equal? (u-mult 8 '(1 0 1 0 1 1 0 1) '(0 1 1 0 1 0 0 1))
        '(0 1 0 0 0 1 1 0 1 1 1 1 0 1 0 1))
