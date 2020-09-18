#lang racket
(require "q1.rkt")
(and 
(equal? (u2b 6 26) '(0 1 1 0 1 0))
(equal? #t
(with-handlers ([exn:fail? (lambda (exn) #t)])
   (u2b 5 1024))))


