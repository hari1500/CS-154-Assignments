#lang racket
(require "q4.rkt")
(equal? (u-sub 8 '(1 1 0 1 0 0 1 0) '(1 0 0 1 1 0 1 0))
 '(0 0 1 1 1 0 0 0))
