#lang racket
(require "syntaxes.rkt")
(require srfi/26) ; cut cute

(provide (struct-out path-node))
(provide path-node-angle)
(provide set-path-node-angle!)
(provide add-path-node-theta!)
(provide path-node-next)
(provide set-path-node-angle!)
(provide path-node-angle)
(provide fill-path-node-control-right!)
(provide path-node-copy-angle)
(provide fill-path-node-control-points!)
(provide fill-path-node-control-left!)
(provide path-node-control-radius-average)



(define (right-control-point point chord theta phi)
  (+ point
     (* chord
        (make-polar 1 theta)
        (/ (velocity theta phi) 3))))

(define (left-control-point point chord theta phi)
  (- point
     (* chord
        (make-polar 1 (- phi))
        (/ (velocity phi theta) 3))))

;mf116
(define (velocity theta phi)
  (let ((ct (cos theta))
        (st (sin theta))
        (cf (cos phi))
        (sf (sin phi))
        (rt5 (sqrt 5)))
    (min 4.0
         (/ (+ 2.0
               (* (sqrt 2) 
                  (- st (/ sf 16)) 
                  (- sf (/ st 16)) 
                  (- ct cf)))
            (+ 1 
               (* 0.5 (- rt5 1) ct)
               (* 0.5 (- 3 rt5) cf))))))

(struct path-node 
  (z 
   chord-left ;  vector from previous node to this one
   chord-right ; vector from this node to next one
   theta ; angle from chord-right to (- control-right z)
   phi ; angle from (- z control-left) to (chord-left)
   ;(equal? 0 (+ phi theta psi))
   ; with psi turn angle from chord-left to chord-right
   control-left 
   control-right)
   ;parent)
  #:mutable
  #:transparent)

(define (path-node-angle pn)
  (let1 chord (path-node-chord-right pn)
        (if (and (not (null? chord)) (not (equal? 0 chord)))
            (+ (angle chord) 
               (path-node-theta pn))
            (angle (- (path-node-control-right pn) (path-node-z pn))))))

(define (set-path-node-angle! pn angle-val)
  (set-path-node-theta! 
   pn 
   (- angle-val 
      (angle (path-node-chord-right pn)))))

(define (add-path-node-theta! pn d-theta)
  (set-path-node-theta! pn (+ (path-node-theta pn) d-theta)))

(define (path-node-next pn-cur pns)
  (let1 pns-cur (member pn-cur pns)
        (if (not (null? (cdr pns-cur)))
            (cadr pns-cur)
            (car pns))))

(define (path-node-copy-angle pn-to pn-from)
  (if (equal? 0 (path-node-chord-right pn-to))
      (set-path-node-control-right! 
       pn-to 
       (path-node-control-right pn-from))
      (set-path-node-angle! pn-to (path-node-angle pn-from)))
  (if (equal? 0 (path-node-chord-left pn-to))
      (set-path-node-control-left! 
       pn-to 
       (path-node-control-left pn-from))
      (set-path-node-phi! 
       pn-to
       (- (angle (path-node-chord-left pn-to))
          (path-node-angle pn-from) 
          ))))

(define (fill-path-node-control-right! pnode-k pnode-k+1)
  (set-path-node-control-right! 
   pnode-k
   (right-control-point (path-node-z pnode-k)
                        (path-node-chord-right pnode-k)
                        (path-node-theta pnode-k)
                        (path-node-phi pnode-k+1))))

(define (fill-path-node-control-left! pnode-k pnode-k+1)
  (set-path-node-control-left! 
   pnode-k+1
   (left-control-point (path-node-z pnode-k+1)
                       (path-node-chord-left pnode-k+1)
                       (path-node-theta pnode-k)
                       (path-node-phi pnode-k+1))))

(define (fill-path-node-control-points! pnode-k pnode-k+1)
  (let ((chord-left (path-node-chord-left pnode-k+1))
        (chord-right (path-node-chord-right pnode-k)))
    (when (not (equal? 0 chord-left))
      (fill-path-node-control-left! pnode-k pnode-k+1))
    (when (not (equal? 0 chord-right))
      (fill-path-node-control-right! pnode-k pnode-k+1))))




(define (path-node-control-radius-average pn)
  (/ (+ (magnitude (- (path-node-control-left pn) (path-node-z pn)))
        (magnitude (- (path-node-control-right pn) (path-node-z pn))))
     2))
