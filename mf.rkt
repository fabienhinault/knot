#lang racket

(require racket/gui)
(require racket/draw)
(require math/matrix)
(require rackunit)

(define-syntax-rule (let1 a b body ...)
    (let ((a b)) body ...))

(define-syntax-rule (w/pen dc  color size body ...)
  (let1 pen (send dc get-pen)
        (send dc set-pen  color size 'solid)
        body ...
        (send dc set-pen pen)))


(define-syntax-rule (w/brush dc size color body ...)
  (let1 brush (send dc get-pen)
        (send dc set-brush color size 'solid)
        body ...
        (send dc set-brush brush)))

  
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
  (z 
   chord-left ;  vector from previous node to this one
   chord-right ; vector from this node to next one
   psi ; turn angle from chord-left to chord-right
   theta ; angle from chord-right to (- control-right z)
   phi ; angle from (- z control-left) to (chord-left)
   ;(equal? 0 (+ phi theta psi))
   control-left 
   control-right 
   parent)
  #:mutable #:transparent)

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

(define (path-node-over? pn)
  (equal? pn (knot-node-over (path-node-parent pn))))

(define (add-path-node-theta! pn d-theta)
  (set-path-node-theta! pn (+ (path-node-theta pn) d-theta)))


(struct knot-node (z first-path-nodes second-path-nodes over) #:mutable #:transparent)

(define (knot-node-first-path-node kn) (car (knot-node-first-path-nodes kn)))
(define (knot-node-second-path-node kn) (car (knot-node-second-path-nodes kn)))
(define (knot-node-path-nodes kn) 
  (list (knot-node-first-path-node kn) (knot-node-second-path-node kn)))
(define (knot-node-other-path-node kn pn)
  (findf (lambda (x) (not (equal? x pn)))
         (knot-node-path-nodes kn)))

(let* ((pns '(1 2 3))
       (kn (knot-node 'z pns (cdr pns) 'none)))
  (check-equal?
   (knot-node-path-nodes kn) '(1 2))
  (check-equal?
   (knot-node-other-path-node kn 2) 1))

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

(define (make-knot2 zs order)
  (let* ((points (map (lambda (i) (list-ref zs i)) order))
         (k (apply make-naked-knot points))
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

(define (knot-8? k)
  (equal? 1 (length (knot-knot-nodes k))))

(define (knot-detect-loop k)
  (let ((pnodes (knot-path-nodes k)))
    (findf (lambda (pns) (equal? (path-node-parent (car pns)) (path-node-parent (cadr pns))))
           (map list pnodes (cycle-left-1 pnodes)))))

(define (knot-detect-pattern-2 k)
  (let ((kn (findf knot-node-is-pattern-2 (knot-knot-nodes k))))
    (if (not kn)
        #f
        (knot-node-is-pattern-2 kn))))

(define (knot-node-is-pattern-2 kn)
  (if (not (null? (cdr (knot-node-first-path-nodes kn))))
      (let* ((pns1 (knot-node-first-path-nodes kn))
             (next-pn1 (cadr pns1))
             (next-kn (path-node-parent next-pn1))
             (pns2 (knot-node-second-path-nodes kn))
             (pns3 (knot-node-first-path-nodes next-kn))
             (pns4 (knot-node-second-path-nodes next-kn)))
        (if (and (or (and (equal? (knot-node-over kn) (car pns1)) (equal? (knot-node-over next-kn) next-pn1))
                     (and (equal? (knot-node-over kn) (car pns2)) (not (equal? (knot-node-over next-kn) next-pn1))))
                 ; find and elegant way to use a combinatorics function
                 (or (and (equal? (cdr pns1) pns3) (equal? (cdr pns2) pns4))
                     (and (equal? (cdr pns1) pns3) (equal? (cdr pns4) pns2))
                     (and (equal? (cdr pns3) pns1) (equal? (cdr pns2) pns4))
                     (and (equal? (cdr pns3) pns1) (equal? (cdr pns4) pns2))))
            (list (car pns1) (car pns3) (car pns2) (car pns4))
            #f))
      #f))

(define (all2lists x y)
  (list (list x y) (list y x)))

(define (path-node-pattern-2? pns1-kn1 pns2-kn1 pns1-kn2 pns2-kn2)
  (and (equal? (cdr pns1-kn1) pns1-kn2) 
       (equal? (cdr pns2-kn1) pns2-kn2)
       (path-node-over? (car pns1-kn1))
       (path-node-over? (car pns1-kn2))))

(define (knot-remove-pattern k pattern)
  (let* ((pnodes (filter (lambda (pn) (not (member pn pattern)))
                         (knot-path-nodes k)))
         (knodes (filter (lambda (kn) (not (member (knot-node-over kn) pattern)))
                         (knot-knot-nodes k)))
         (points (map path-node-z pnodes))
         (chords (cycle-map-minus points))
         (res (apply make-naked-knot points))
         (res-pnodes (knot-path-nodes res)))
    (map set-path-node-chord-right! res-pnodes chords)
    (map set-path-node-chord-left! res-pnodes (cycle-right-1 chords))
    (map path-node-copy-angle res-pnodes pnodes)
    (map knot-node-copy-over
         (knot-knot-nodes res)
         knodes)
    (map fill-path-node-control-points! res-pnodes (cycle-left-1 res-pnodes))
    (knot-fill-path! res)
    res))

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

(define (knot-node-copy-over kn-to kn-from)
  (set-knot-node-over!
   kn-to 
   (if (equal? 'none  (knot-node-over kn-from))
       'none
       (minf (knot-node-path-nodes kn-to)
             (lambda (pn) 
               (abs (- (path-node-angle pn)
                       (path-node-angle (knot-node-over kn-from)))))))))

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

(define (knot-draw k dc)
  (let ((pen (send dc get-pen)))
    (send dc set-pen "black" 5 'solid)
    (send dc draw-path (knot-path k))
    (for ([knode (knot-knot-nodes k)])
      (when (not (equal? 'none (knot-node-over knode)))
        (let* ((pnode (knot-node-over knode))
               (z (path-node-z pnode))
               (z-angle (make-polar 1 (path-node-angle pnode))))
          (send dc set-pen "white" 15 'solid)
          (draw-z-line dc z (* 5 z-angle))
          (send dc set-pen "black" 5 'solid)
          (draw-z-line dc z (* 10 z-angle)))))
    (send dc set-pen pen)))

(define (turnAngle chord1 chord2)
  (if (or (equal? 0 chord1) (equal? 0 chord2))
      '()
      (-mod (* 2 pi) (angle chord2) (angle chord1))))
; I don't know which one is more efficient, this one or
;  (angle (/ chord2 chord1)))

(define (-mod x angle1 angle2)
  (* x
     (- ((λ(_) (- _ (floor _)))
         (+ 1/2 (/ (- angle1 angle2) x)))
        1/2)))

(check-equal? 
 (-mod (* 2 pi) (- (* 0.75 pi)) (* 0.75 pi))
 (* 0.5 pi))

(check-equal? 
 (-mod 1 0 0.25)
 -0.25)

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
  (let* ((pn1 (car (knot-node-first-path-nodes kn)))
         (pn2 (car (knot-node-second-path-nodes kn)))
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

(define *knot* 
  (make-knot2 '(100+200i 200+100i 200+300i 300+200i 400+100i 400+300i 500+200i)
             '(0 1 4 6 5 3 1 0 2 5 6 4 3 2)))

(define (get-path-node kn angle)
  (let* ((pn1 (car (knot-node-first-path-nodes kn)))
         (angle1 (path-node-angle pn1))
         (pn2 (car (knot-node-second-path-nodes kn)))
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
  (let ((nearest (minf
                  (knot-knot-nodes aknot)
                  (lambda (k-node) (magnitude (- (knot-node-z k-node) (make-rectangular x y)))))))
    (if (equal? (knot-node-over nearest) 'none)
        nearest
        '())))

(let* ((k  (make-knot2 '(100+200i 200+100i 200+300i 300+200i 400+100i 400+300i 500+200i)
                       '(0 1 4 6 5 3 1 0 2 5 6 4 3 2)))
       (kn100+200i (findf (lambda (kn) (equal? (knot-node-z kn) 100+200i)) (knot-knot-nodes k))))
  (check-equal? 
   (get-nearest-node 
    50 200 
    k)
   kn100+200i)
  (set-knot-node-over! kn100+200i (knot-node-first-path-node kn100+200i))
  (check-equal? 
   (get-nearest-node 
    50 200 
    k)
   '()))


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


(define (knot-finished? k)
  (andmap (lambda (kn) (not (equal? 'none (knot-node-over kn))))
          (knot-knot-nodes k)))

(define (knot-game-over? k)
  (equal? #f (findf (lambda (kn) (equal? (knot-node-over kn) 'none))
                      (knot-knot-nodes k))))

(define (dumb-computer-play k)
  (let ((knode (findf (lambda (kn) (equal? (knot-node-over kn) 'none))
                      (knot-knot-nodes k))))
    (when knode
      (set-knot-node-over! knode (car (knot-node-first-path-nodes knode))))))

(define (random-list-ref l)
  (let1 len (length l)
        (list-ref l (random len))))

(define (random-computer-play k)
  (let1 
   knode
   (random-list-ref 
    (filter 
     (lambda (kn) (equal? (knot-node-over kn) 'none))
     (knot-knot-nodes k)))
   (when knode
      (set-knot-node-over! 
       knode 
       (car ((if (equal? 0 (random 2))
                knot-node-first-path-nodes
                knot-node-second-path-nodes)
             knode))))))

(define (2-players-play k) '())

(define (path-node-control-radius-average pn)
  (/ (+ (magnitude (- (path-node-control-left pn) (path-node-z pn)))
        (magnitude (- (path-node-control-right pn) (path-node-z pn))))
     2))

(define kg-canvas%
  (class canvas%
    (init aknot 2nd-player-play)
    (define mknot aknot)
    (define m2nd-player-play 2nd-player-play)
    (super-new)
    (field [pnode '()]
           [circle '()])
    
    (define/override (on-paint)
      (let ((dc (send this get-dc)))
        (when (not (null? mknot))
          (knot-draw mknot dc))
        (when (not (null? pnode))
          (let* ((brush (send dc get-brush))
                 (pen (send dc get-pen))
                 (z (path-node-z pnode))
                 (z-angle (make-polar 1 (path-node-angle pnode))))
            (send dc set-pen "yellow" 10 'solid)
            (draw-z-line dc z (* 10 z-angle))
            (send dc set-pen "black" 5 'solid)
            (draw-z-line dc z (* 10 z-angle))
            (send dc set-brush brush)
            (send dc set-pen pen)))
        (when (not (null? circle))
          (let ((center (car circle))
                (radius (cadr circle)))
            (w/pen dc "black" 5 'solid
                   (send dc draw-ellipse 
                         (- (real-part center) (/ radius 2))
                         (- (imag-part (car circle)) (/ radius 2))
                         radius
                         radius))))))
    
    
    (define/override (on-event event)
      (let ((event-type (send event get-event-type)))
        (case event-type
          ['motion
           (when (not (null? mknot))
             (let* ((x (send event get-x))
                    (y (send event get-y))
                    (z (make-rectangular x y))
                    (knode (get-nearest-node x y mknot))
                    (knode-z (if (not (null? knode)) (knot-node-z knode) '())))
               (if (and (not (null? knode-z)) (not (equal? (- knode-z z) 0)))
                   (set! pnode (get-path-node knode (angle (- z knode-z))))
                   (set! pnode '()))
               (send this refresh)))]
          [(left-up)
           (when (not (null? pnode))
             (set-knot-node-over! (path-node-parent pnode) pnode)
             (if (knot-game-over? mknot)
                 (send this solve)
                 (m2nd-player-play mknot))
             (set! pnode '())
             (send this refresh)
             )]
          )))
    
    (define/public (blink f)
      (let1 dc (send this get-dc)
            (f dc)
            (sleep 0.5)
            (send this flush)
            (send dc clear)
            (knot-draw mknot dc)
            (sleep 0.5)
            (send this flush)
            (f dc)           
            (sleep 0.5)))
    
    
    (define/public (solve)
      (send this refresh)
      (yield)
      (let* ((p2 (knot-detect-pattern-2 mknot))
             (p1 (knot-detect-loop mknot)))
      (cond ((knot-8? mknot)
             (send this blink
                   (λ (dc)
                     (w/pen dc "yellow" 20 'solid
                            (draw-z-point dc (knot-node-z (car (knot-knot-nodes mknot)))))))
             (set! circle 
                   (list (knot-node-z (car (knot-knot-nodes mknot)))
                         (/ (+ (path-node-control-radius-average 
                                (car (knot-path-nodes mknot)))
                               (path-node-control-radius-average 
                                (cadr (knot-path-nodes mknot))))
                            2)))
             (set! mknot '())
             (send this refresh))
            (p1
             (send this blink
                   (λ (dc)
                     (w/pen dc "yellow" 20 'solid
                            (draw-z-point dc (path-node-z (car p1))))))
             (set! mknot (knot-remove-pattern mknot p1))
             (send this refresh)
             (send this solve))
            (p2
             (send this blink
                   (λ (dc)
                   (let ((kn1 (path-node-parent (car p2)))
                         (kn2 (path-node-parent (cadr p2))))
                     (w/pen dc "yellow" 20 'solid  
                            (draw-z-point dc (knot-node-z kn1))
                            (draw-z-point dc (knot-node-z kn2))))))
           (set! mknot (knot-remove-pattern mknot p2))
           (send this refresh)
           (send this solve)))))      
    ))

(define (start 2nd-player-play)
  (let* ((frame (new frame%
                     [label "To knot or not to knot"]
                     [width 600]
                     [height 450]))
         (menu-bar (new menu-bar%
                      (parent frame)))
         (canvas
          (new kg-canvas%
               [parent frame]
               [aknot (make-knot2 
                       '(100+200i 
                         200+100i 
                         200+300i 
                         300+200i 
                         400+100i 
                         400+300i 
                         500+200i)
                       '(0 1 4 6 5 3 1 0 2 5 6 4 3 2))]
               [2nd-player-play 2nd-player-play])))
    (new menu%
         (label "&Game")
         (parent menu-bar))
    (send frame show #t)
    frame))

(define frame (start 2-players-play))