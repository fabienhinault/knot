#lang racket
(require racket/gui)
(require racket/draw)
(require math/matrix)


(define (int r) (inexact->exact (round r)))
(define (cycle-map f l)
  (map f (append (cdr l) (list (car l)))  l ))
(define (cycle-map-minus l)
  (cycle-map - l))


;mf11

;(define z1 100+100i)
;(define z2 300+100i)
;(define z3 200+200i)
;(define z4 200+300i)
;(define knots 
;  (list z1 z2 z3 z4 z2 z1 z4 z3))
;  (list 100+100i 300+100i 300+200i))
;  (list 100+100i 300+100i))

(define z1 100+200i)
(define z2 200+100i)
(define z3 200+300i)
(define z4 300+200i)
(define z5 400+100i)
(define z6 400+300i)
(define z7 500+200i)

(define knots 
  (list z1 z2 z5 z7 z6 z4 z2 z1 z3 z6 z7 z5 z4 z3))

(define n (length knots))
(define deltas (cycle-map-minus knots))
(define psis 
  (let ((l (cycle-map (lambda (d-k-k+1 d-k+1-k+2) (angle (/ d-k+1-k+2 d-k-k+1))) deltas)))
    (cons (last l) (drop-right l 1))))


;mf116
(define (velocity theta phi)
  (min 4.0
        (+ 2.0
           (* (sqrt 2) 
              (- (sin theta) (/ (sin phi) 16)) 
              (- (sin phi) (/ (sin theta) 16)) 
              (- (cos theta) (cos phi))))
        (* 3.0 (+ 1 (* 0.5 (- (sqrt 5) 1) (cos theta))
                  (* 0.5 (- 3 (sqrt 5)) (cos phi))))))

(define (prev i)
  (modulo (sub1 i) n))
(define (next i)
  (modulo (add1 i) n))

;mf276
(define (A i j)
  (if (not (eq? j (prev i)))
      0
      (/ 1 (magnitude (list-ref deltas j)))))
(define (B+C i j)
  (if (not (eq? i j))
      0
      (+ (/ 2 (magnitude (list-ref deltas (prev i))))
         (/ 2 (magnitude (list-ref deltas i))))))
(define (D i j)
  (if (not (eq? j (next i)))
      0
      (/ 1 (magnitude (list-ref deltas i)))))

(define right
  (build-matrix 
   n 1
   (lambda (i j) 
     (- 0
        (* (/ 2 (magnitude (list-ref deltas (prev i)))) 
           (list-ref psis i))
        (* (/ 1 (magnitude (list-ref deltas i))) 
           (list-ref psis (next i)))))))

(define mat
  (build-matrix n n
                (lambda (i j) (+ (A i j) (B+C i j) (D i j)))))

(define thetas
  (matrix->list (matrix-solve mat right)))

(define phis
  (map (lambda (theta psi) (- 0 theta psi))
       thetas
       psis))

(define (right-control-point index)
  (+ (list-ref knots index)
     (* (list-ref deltas index)
        (make-polar 1 (- (list-ref thetas index)))
        (/ (velocity (list-ref thetas index) (list-ref phis (next index)))
           3))))

(define (left-control-point index)
  (- (list-ref knots index)
     (* (list-ref deltas (prev index))
        (make-polar 1 (list-ref phis index))
        (/ (velocity (list-ref phis index) (list-ref thetas (prev index)))
           3))))



(define z-path%
  (class dc-path%
    (define (z-curve-to z1 z2 z3)
      (send this curve-to
            (real-part z1)
            (imag-part z1)
            (real-part z2)
            (imag-part z2)
            (real-part z3)
            (imag-part z3)))
    (define (z-move-to z)
      (send this move-to
            (real-part z)
            (imag-part z)))
    (public z-curve-to)
    (public z-move-to)
    (super-new)))

(define path (new z-path%))
(send path z-move-to (car knots))
(for ([i (range n)])
;(for ([i (range 2)])
  
  (send path z-curve-to
   (right-control-point i)
   (left-control-point (next i))
   (list-ref knots (next i))))
        
(define frame (new frame%
                   [label "Example"]
                   [width 500]
                   [height 500]))
(define canvas
  (new canvas% [parent frame]
       [paint-callback
        (lambda (canvas dc)
                (send dc draw-path path))]))
      