#lang racket

(provide minf)
(provide draw-z-line)

(define (minf l f)
  (define (aux l f val res)
    (if (null? l)
        res
        (let ((valcar (f (car l))))
          (if (< val valcar)
              (aux (cdr l) f val res)
              (aux (cdr l) f valcar (car l))))))
  (aux l f +inf.0 '()))

(define (draw-z-line dc z z-delta)
  (let ((z-start (- z z-delta))
        (z-end   (+ z z-delta)))
    (send dc draw-line
          (real-part z-start)
          (imag-part z-start)
          (real-part z-end)
          (imag-part z-end))))