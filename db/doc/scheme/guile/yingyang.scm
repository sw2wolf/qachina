#! /usr/local/bin/guile -s
!#
(define call/cc call-with-current-continuation)
(define n 0)
(define bar (lambda (bar) bar))
(define foo (lambda (foo) (display n) (newline) (set! n (+ n 1)) foo))
((call/cc bar) (foo (call/cc bar)))
