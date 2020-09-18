#lang racket
(require "q2.rkt")
(and 
(equal? (s2b 6 26) '(0  1 1 0 1 0))
(equal? #t
(with-handlers ([exn:fail? (lambda (exn) #t)])
   (s2b 5 16))))
