#lang racket

(provide let1)
(provide w/pen)

(define-syntax-rule (let1 a b body ...)
    (let ((a b)) body ...))

(define-syntax-rule (w/pen dc  color size body ...)
  (let1 pen (send dc get-pen)
        (send dc set-pen  color size 'solid)
        body ...
        (send dc set-pen pen)))
