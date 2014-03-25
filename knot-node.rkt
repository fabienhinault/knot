#lang racket

(require rackunit)
(require "utils.rkt")
(require "path-node.rkt")

(provide (struct-out knot-node))
(provide knot-node-path-nodes)
(provide knot-node-copy-over)
(provide knot-node-tweak-thetas!)
(provide knot-node-angle-path-node)
(provide knot-node-none-over?)
(provide knot-node-parent?)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(struct knot-node (z first-path-node second-path-node over) #:mutable #:transparent)

(define (knot-node-path-nodes kn) 
  (list (knot-node-first-path-node kn) (knot-node-second-path-node kn)))
;(define (knot-node-other-path-node kn pn)
;  (findf (lambda (x) (not (equal? x pn)))
;         (knot-node-path-nodes kn)))

(let* ((pns '(1 2 3))
       (kn (knot-node 'z (car pns) (cadr pns) 'none)))
  (check-equal?
   (knot-node-path-nodes kn) '(1 2)))
;  (check-equal?
;   (knot-node-other-path-node kn 2) 1))

(define (knot-node-copy-over kn-to kn-from)
  (set-knot-node-over!
   kn-to 
   (if (equal? 'none  (knot-node-over kn-from))
       'none
       (minf (knot-node-path-nodes kn-to)
             (lambda (pn) 
               (abs (- (path-node-angle pn)
                       (path-node-angle (knot-node-over kn-from)))))))))


(define (tweak-angles angle1 angle2)
; tweaks 2 angles so that they cross orthogonally
  (let* ((offset (- angle2 angle1))
         (target-offset (+ (/ pi 2) 
                           (* (floor (/ offset pi)) 
                              pi))))
    (/ (- offset target-offset) 2)))

(check-equal?
 (+ -1.4 (tweak-angles -1.4 1.4))
 (- (/ pi 4)))

(check-equal?
 (+ -1.450515964828205 (tweak-angles -1.450515964828205 1.450515964828205))
 (- (/ pi 4)))

(check-equal?
 (+ 1.4 (tweak-angles 1.4 -1.4))
 (/ pi 4))

(check-equal?
 (< (tweak-angles -0.3345558925337896 3.288158208922769)  0 )
 #true)

(define (knot-node-tweak-thetas! kn)
  (let* ((pn1 (knot-node-first-path-node kn))
         (pn2 (knot-node-second-path-node kn))
         (tweak (tweak-angles  (path-node-angle pn1) (path-node-angle pn2))))
    (add-path-node-theta! pn1 (+ tweak))
    (add-path-node-theta! pn2 (- tweak))))

(define (knot-node-angle-path-node kn angle)
  (let* ((pn1 (knot-node-first-path-node kn))
         (angle1 (path-node-angle pn1))
         (pn2 (knot-node-second-path-node kn))
         (angle2 (path-node-angle pn2)))
    (if (< (abs (cos (- angle angle1)))
           (abs (cos (- angle angle2))))
        pn2
        pn1)))


(define (knot-node-none-over? kn)
  (equal? 'none (knot-node-over kn)))

(define (knot-node-parent? kn pn)
  (or (equal? pn (knot-node-first-path-node kn))
      (equal? pn (knot-node-second-path-node kn))))
