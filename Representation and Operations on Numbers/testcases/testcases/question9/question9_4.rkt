#lang racket
(require "q9.rkt")
(and 
(equal? (d2fix 6 18.5) '(0 1 0 0 1 0 1 0 0 0 0 0))
(equal? #t
(with-handlers ([exn:fail? (lambda (exn) #t)])
   (d2fix 6 1024.2))))
