#lang racket

(require "syntaxes.rkt")

(provide cycle-left-1)
(provide left-cycle-1)
(provide cycle-right-1)
(provide right-cycle-1)
(provide cycle-map-minus)

(provide minf)
(provide random-list-ref)
(provide draw-z-line)
(provide draw-z-point)


(define (cycle-left-1 l)
  (append (cdr l) (list (car l))))
(define left-cycle-1 cycle-left-1)
(define (cycle-right-1 l)
  (cons (last l) (drop-right l 1)))
(define right-cycle-1 cycle-right-1)
(define (cycle-map f l)
  (map f (cycle-left-1 l)  l ))
(define (cycle-map-minus l)
  (cycle-map - l))


(define (minf l f)
  (define (aux l f val res)
    (if (null? l)
        res
        (let ((valcar (f (car l))))
          (if (< val valcar)
              (aux (cdr l) f val res)
              (aux (cdr l) f valcar (car l))))))
  (aux l f +inf.0 '()))


(define (random-list-ref l)
  (let1 len (length l)
        (list-ref l (random len))))


(define (draw-z-line dc z z-delta)
  (let ((z-start (- z z-delta))
        (z-end   (+ z z-delta)))
    (send dc draw-line
          (real-part z-start)
          (imag-part z-start)
          (real-part z-end)
          (imag-part z-end))))


(define (draw-z-point dc z)
  (send dc draw-point (real-part z) (imag-part z)))
