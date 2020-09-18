#lang racket
(require "q5.rkt")
(equal? (u-mult 4 '(1 1 1 1) '(1 1 1 1))
        '(1 1 1 0 0 0 0 1))
