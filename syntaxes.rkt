#lang racket

(provide let1)

(define-syntax-rule (let1 a b body ...)
    (let ((a b)) body ...))
