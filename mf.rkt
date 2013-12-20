#lang racket

(require racket/gui)
(require racket/draw)
(require math/matrix)
(require rackunit)


(define (cycle-left-1 l)
  (append (cdr l) (list (car l))))
(define left-cycle-1 cycle-left-1)
(define (cycle-right-1 l)
  (cons (last l) (drop-right l 1)))
(define right-cycle-1 cycle-left-1)
(define (cycle-map f l)
  (map f (cycle-left-1 l)  l ))
(define (cycle-map-minus l)
  (cycle-map - l))

(struct path-node 
  (z chord-left chord-right psi theta phi control-left control-right parent)
  #:mutable #:transparent)

(define (path-node-angle pn)
  (+ (angle (path-node-chord-right pn)) 
     (path-node-theta pn)))

(define (add-path-node-theta! pn d-theta)
  (set-path-node-theta! pn (+ (path-node-theta pn) d-theta)))

(struct knot-node (z first-path-node second-path-node over) #:mutable #:transparent)

(struct knot (knot-nodes path-nodes path)  #:mutable #:transparent)

(define (make-naked-knot . points)
  (define (make-knot-node pnodes)
    (cond ((null? pnodes) '())
          (else
           (let* ((z (path-node-z (car pnodes)))
                  (pnodes2 (memf (lambda (pn) (equal? z (path-node-z pn))) (cdr pnodes)))
                  (knode (knot-node z pnodes pnodes2 'none)))
             (set-path-node-parent! (car pnodes) knode)
             (set-path-node-parent! (car pnodes2) knode)
             knode))))
  
  (define (make-knot-nodes pnodes)
    (cond ((null? pnodes) '())
          ((not (null? (path-node-parent (car pnodes))))
           (make-knot-nodes (cdr pnodes)))
          (else        
           (cons (make-knot-node pnodes) (make-knot-nodes (cdr pnodes))))))
                  
  (let ((path-nodes (map 
                     (lambda (z) (path-node z '() '() '() '() '() '() '() '()))
                     points)))
    (knot (make-knot-nodes path-nodes) path-nodes '())))
        

; many things borrowed from
; http://hackage.haskell.org/package/cubicbezier-0.2.0/docs/Geom2D-CubicBezier-MetaPath.html
(define (make-knot . points)
  (let* ((k (apply make-naked-knot points))
         (chords (cycle-map-minus points))
         (turnAngles (map turnAngle (cycle-right-1 chords) chords))
         (orig-thetas (matrix->list 
                       (matrix-solve 
                        (knot-matrix chords)
                        (knot-right-vector chords turnAngles))))
         (pnodes (knot-path-nodes k)))
    (map set-path-node-chord-right! pnodes chords)
    (map set-path-node-chord-left! pnodes (cycle-right-1 chords))
    (map set-path-node-theta! pnodes orig-thetas)
    (map knot-node-tweak-thetas! (knot-knot-nodes k))
    (knot-fill-phis! k turnAngles)
    (map fill-path-node-control-points! pnodes (cycle-left-1 pnodes))
    (knot-fill-path! k)
    k))

;(define (knot-solve k)
;  (let ((pnodes (knot-path-nodes k)))
;    (for ([pn1 pnodes]
;          [pn2 (cycle-left-1 pnodes)])
;      (if (equal? (path-node-parent pn1) (path-node-parent pn2))
;          (let ((knode (path-node-parent pn1)))
;            (set-path-node-parent pn1 '())
;            (set-path-node-parent pn2 '())
;            (knot (filter (lambda (kn) (not (equal? kn knode)))
;                          (knot-knote-nodes k))
;                  (filter (lambda (pn) (not (or (equal? pn pn1)(equal? pn pn2))))
;                          (knot-path-nodes k))
;                  (knot-path k))
;                   '())))))
          
(define (knot-detect-loop k)
  (let ((pnodes (knot-path-nodes k)))
    (findf (lambda (pns) (equal? (path-node-parent (car pns)) (path-node-parent (cadr pns))))
           (map list pnodes (cycle-left-1 pnodes)))))
        
(define (knot-remove-loop k knot-loop)
  (let* ((pnodes (filter (lambda (pn) (not (member knot-loop)))
                         (knot-path-nodes k))))
         (apply make-knot (map path-node-z (map car pnodes)))))
  
(define (knot-detect-pattern-2 k)
  (let ((kn (findf knot-node-is-pattern-2 (knot-knot-nodes k))))
    (knot-node-is-pattern-2 kn)))
                
(define (knot-node-is-pattern-2 kn)
  (if (not (null? (cdr (knot-node-first-path-node kn))))
      (let* ((next-pn (cadr (knot-node-first-path-node kn)))
             (next-kn (path-node-parent next-pn))
             (pn1 (knot-node-first-path-node kn))
             (pn2 (knot-node-second-path-node kn))
             (pn3 (knot-node-first-path-node next-kn))
             (pn4 (knot-node-second-path-node next-kn)))
        (if (or (and (equal? (cdr pn1) pn3) (equal? (cdr pn2) pn4))
                (and (equal? (cdr pn1) pn3) (equal? (cdr pn4) pn2))
                (and (equal? (cdr pn3) pn1) (equal? (cdr pn2) pn4))
                (and (equal? (cdr pn3) pn1) (equal? (cdr pn4) pn2)))
            (list (car pn1) (car pn3) (car pn2) (car pn4))
            #f))
      #f))

(define (knot-remove-pattern-2 k pattern)
   (let* ((pnodes (filter (lambda (pn) (not (member pn pattern)))
                          (knot-path-nodes k))))
     (apply make-knot (map path-node-z pnodes))))

(define (knot-fill-path! k)
  (let ((z-path (new z-path%))
        (pnodes (knot-path-nodes k)))
    (send z-path z-move-to (path-node-z (car (knot-path-nodes k))))
    (for ([pnode-k pnodes]
          [pnode-k+1 (left-cycle-1 pnodes)])
      (send z-path z-curve-to (path-node-control-right pnode-k)
            (path-node-control-left pnode-k+1)
            (path-node-z pnode-k+1)))
    (set-knot-path! k z-path)))

(define (fill-path-node-control-points! pnode-k pnode-k+1)
  (set-path-node-control-right! 
   pnode-k
   (right-control-point (path-node-z pnode-k)
                        (path-node-chord-right pnode-k)
                        (path-node-theta pnode-k)
                        (path-node-phi pnode-k+1)))
  (set-path-node-control-left! 
   pnode-k+1
   (left-control-point (path-node-z pnode-k+1)
                       (path-node-chord-left pnode-k+1)
                       (path-node-theta pnode-k)
                       (path-node-phi pnode-k+1))))

(define (knot-draw k dc)
  (let ((pen (send dc get-pen)))
    (send dc set-pen "black" 5 'solid)
    (send dc draw-path (knot-path k))
    (for ([knode (knot-knot-nodes k)])
      (when (not (equal? 'none (knot-node-over knode)))
        (let* ((pnode (knot-node-over knode))
               (z (path-node-z pnode))
               (z-angle (make-polar 1 (path-node-angle pnode))))
          (send dc set-pen "white" 20 'solid)
          (draw-z-line dc z z-angle)
          (send dc set-pen "black" 5 'solid)
          (draw-z-line dc z (* 10 z-angle)))))
    (send dc set-pen pen)))


(define (turnAngle chord1 chord2)
  (angle (/ chord2 chord1)))

;mf276
(define (knot-matrix chords)
  (define n (length chords))
  (define (prev i)
    (modulo (sub1 i) n))
  (define (next i)
    (modulo (add1 i) n))
  (define (A i j)
    (if (not (eq? j (prev i)))
        0
        (/ 1 (magnitude (list-ref chords j)))))
  (define (B+C i j)
    (if (not (eq? i j))
        0
        (+ (/ 2 (magnitude (list-ref chords (prev i))))
           (/ 2 (magnitude (list-ref chords i))))))
  (define (D i j)
    (if (not (eq? j (next i)))
        0
        (/ 1 (magnitude (list-ref chords i)))))
  (let ((n (length chords)))
    (build-matrix 
     n n
     (lambda (i j) (+ (A i j) (B+C i j) (D i j))))))

(define (knot-right-vector chords turnAngles)
  (define n (length chords))
  (define (prev i)
    (modulo (sub1 i) n))
  (define (next i)
    (modulo (add1 i) n))
  (build-matrix 
   (length chords) 1
   (lambda (i j) 
     (- 0
        (* (/ 2 (magnitude (list-ref chords (prev i)))) 
           (list-ref turnAngles i))
        (* (/ 1 (magnitude (list-ref chords i))) 
           (list-ref turnAngles (next i)))))))

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

(define (knot-fill-phis! k turnAngles)
  (map 
   (lambda (pnode turnAngle)
     (set-path-node-phi! pnode (- 0 (path-node-theta pnode) turnAngle)))
   (knot-path-nodes k)
   turnAngles))

(define (knot-tweak-thetas zs thetas)
  (let ((orig-k (apply make-knot zs)))
    (map set-path-node-theta! (knot-path-nodes orig-k) thetas)
    (knot-fill-chords! orig-k)
    (map knot-node-tweak-thetas! (knot-knot-nodes orig-k))
    (map path-node-theta (knot-path-nodes orig-k))))

(define z1 100+200i)
(define z2 200+100i)
(define z3 200+300i)
(define z4 300+200i)
(define z5 400+100i)
(define z6 400+300i)
(define z7 500+200i)


;(define z1 100+200i )
;(define z2 300+200i )
;(define z3 200+258i)
;(define z4 200+373i )


(define (knot-fill-chords! k) 
  (let* ((pnodes (knot-path-nodes k))
         (zs (map path-node-z pnodes))
         (chords (cycle-map-minus zs)))
    (map set-path-node-chord-right! pnodes chords)
    (map set-path-node-chord-left! pnodes (cycle-right-1 chords))))



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





(define (tweak-angles angle1 angle2)
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
  (let* ((pn1 (car (knot-node-first-path-node kn)))
         (pn2 (car (knot-node-second-path-node kn)))
         (tweak (tweak-angles  (path-node-angle pn1) (path-node-angle pn2))))
    (add-path-node-theta! pn1 (+ tweak))
    (add-path-node-theta! pn2 (- tweak))))



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

(define *knot* (make-knot z1 z2 z5 z7 z6 z4 z2 z1 z3 z6 z7 z5 z4 z3))

(define (start)
  (let* ((frame (new frame%
                   [label "Example"]
                   [width 600]
                   [height 450]))
         (canvas
          (new kg-canvas%
               [parent frame]
               [aknot *knot*])))
    (send frame show #t)))
    

(define (get-path-node kn angle)
  (let* ((pn1 (car (knot-node-first-path-node kn)))
         (angle1 (path-node-angle pn1))
         (pn2 (car (knot-node-second-path-node kn)))
         (angle2 (path-node-angle pn2)))
    (if (< (abs (cos (- angle angle1)))
           (abs (cos (- angle angle2))))
        pn2
        pn1)))


(define (minf l f)
  (define (aux l f val res)
    (if (null? l)
        res
        (let ((valcar (f (car l))))
          (if (< val valcar)
              (aux (cdr l) f val res)
              (aux (cdr l) f valcar (car l))))))
  (aux l f +inf.0 '()))

(define (get-nearest-node x y aknot)
  (minf (knot-knot-nodes aknot)
        (lambda (k-node) (magnitude (- (knot-node-z k-node) (make-rectangular x y))))))

(define (draw-z-line dc z z-delta)
  (let ((z-start (- z z-delta))
        (z-end   (+ z z-delta)))
    (send dc draw-line
          (real-part z-start)
          (imag-part z-start)
          (real-part z-end)
          (imag-part z-end))))

(define (knot-finished? k)
  (andmap (lambda (kn) (not (equal? 'none (knot-node-over kn))))
          (knot-knot-nodes k)))

(define (computer-play k)
  (let ((knode (findf (lambda (kn) (equal? (knot-node-over kn) 'none))
                      (knot-knot-nodes k))))
    (when knode
      (set-knot-node-over! knode (car (knot-node-first-path-node knode))))))

(define kg-canvas%
  (class canvas%
    (init aknot)
    (define mknot aknot)
    ;    (define mpath (apply knot-path (map path-node-z (knot-path-nodes mknot))))
    (define mpath (knot-path mknot))
    (super-new)
    (field [x 0]
           [y 0]
           [z 0]
           [knode '()]
           [pnode '()])
    
    
    (define/override (on-paint)
      (let ((dc (send this get-dc)))
        (knot-draw mknot dc)
        (when (not (null? knode))
          (send dc set-brush "black" 'opaque)
          (send dc draw-ellipse
                   (- (real-part z) 5)
                   (- (imag-part z) 5)
                   10
                   10)
          (send dc set-brush "black" 'transparent)))
      )
    
    
    (define/override (on-event event)
      (let ((event-type (send event get-event-type)))
        (case event-type
          ['left-down (set! x (send event get-x))
                      (set! y (send event get-y))
                      (set! knode (get-nearest-node x y mknot))
                      (set! z (knot-node-z knode))
                      (set! pnode '())
                      (send this refresh)]
          [(left-up)
           (let ((mouse-z (make-rectangular
                           (send event get-x)
                           (send event get-y))))
             (when (and (equal? knode 
                                (get-nearest-node 
                                 (send event get-x)
                                 (send event get-y)
                                 mknot))
                        (not (equal? (- mouse-z z) 0)))
               
               (set-knot-node-over! knode (get-path-node knode (angle (- mouse-z z))))
               (computer-play mknot)
               (set! knode '())
               (send this refresh)
               ))]
          )))))

(start)