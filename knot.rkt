#lang racket

(require math/matrix)
(require racket/gui)
(require rackunit)
(require (only-in srfi/1 list-index)) ; list-index
(require srfi/26) ; cut cute
(require "syntaxes.rkt")
(require "utils.rkt")
(require "path-node.rkt")
(require "knot-node.rkt")

(provide (struct-out knot))
(provide knot-complete?)
(provide knot-play)
(provide knot-trace)
(provide knot-0?)
(provide knot-8?)
;(provide make-shadow-trefoil)
(provide knot-knotting?)
(provide knot-unknotting?)
(provide xknotting-results)
(provide knot-draw)
(provide knot-detect-pattern-2)
(provide knot-detect-loop)
(provide knot-remove-pattern)
(provide knot-nearest-node)
(provide make-shadow-7-4)

(provide path-node-parent)
;(provide 
;(provide 
;(provide 
;(provide 

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


(define (turnAngle chord1 chord2)
  (if (or (equal? 0 chord1) (equal? 0 chord2))
      '()
      (-mod (* 2 pi) (angle chord2) (angle chord1))))

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




;;;
;knot



(define (path-node-over? pn kns)
  (equal? pn (knot-node-over (path-node-parent pn kns))))


(define (path-node-parent pn kns)
  (findf (cut knot-node-parent? <> pn)
         kns))

(define (path-node-parent-index pn kns)
  (list-index (cut knot-node-parent? <> pn) kns))



;knot drawing math utils
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
  (build-matrix 
     n n
     (lambda (i j) (+ (A i j) (B+C i j) (D i j)))))

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


;knot graphic utils
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

(struct knot (knot-nodes path-nodes path)  #:mutable #:transparent)

(define (make-naked-knot . points)
  (define (make-knot-node pnodes)
    (cond ((null? pnodes) '())
          (else
           (let* ((pnode (car pnodes))
                  (z (path-node-z pnode))
                  (pnodes2 (memf (lambda (pn) (equal? z (path-node-z pn))) (cdr pnodes)))
                  (knode (knot-node z pnode (car pnodes2) 'none)))
;             (set-path-node-parent! pnode knode)
;             (set-path-node-parent! (car pnodes2) knode)
             knode))))
  
  
  (define (make-knot-nodes pnodes knodes)
    (cond ((null? pnodes) knodes)
          ((findf (lambda (kn) (member (car pnodes) (knot-node-path-nodes kn)))
                  knodes)
           (make-knot-nodes (cdr pnodes) knodes))
          (else
           (make-knot-nodes (cdr pnodes) (cons (make-knot-node pnodes) knodes)))))
  
  (let* ((path-nodes (map 
                     (lambda (z) (path-node z '() '() '() '() '() '() ))
                     points)))
    (knot (make-knot-nodes path-nodes '()) path-nodes '())))


; many things borrowed from
; http://hackage.haskell.org/package/cubicbezier-0.2.0/docs/Geom2D-CubicBezier-MetaPath.html
; ftp://db.stanford.edu/pub/cstr/reports/cs/tr/85/1047
; http://texdoc.net/texmf-dist/doc/generic/knuth/mf/mf.pdf

(define (make-knot zs order)
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

(define (knot-fill-phis! k turnAngles)
  (map 
   (lambda (pnode turnAngle)
     (set-path-node-phi! pnode (- 0 (path-node-theta pnode) turnAngle)))
   (knot-path-nodes k)
   turnAngles))

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

(define (knot-nearest-node x y aknot)
  (let ((nearest (minf
                  (knot-knot-nodes aknot)
                  (lambda (k-node) (magnitude (- (knot-node-z k-node) (make-rectangular x y)))))))
    (if (equal? (knot-node-over nearest) 'none)
        nearest
        '())))

(let* ((k  (make-knot '(100+200i 200+100i 200+300i 300+200i 400+100i 400+300i 500+200i)
                       '(0 1 4 6 5 3 1 0 2 5 6 4 3 2)))
       (kn100+200i (findf (lambda (kn) (equal? (knot-node-z kn) 100+200i)) (knot-knot-nodes k))))
  (check-equal? 
   (knot-nearest-node 
    50 200 
    k)
   kn100+200i)
  (set-knot-node-over! kn100+200i (knot-node-first-path-node kn100+200i))
  (check-equal? 
   (knot-nearest-node 
    50 200 
    k)
   '()))

(define (knot-complete? k)
  (equal? #f (findf (lambda (kn) (equal? (knot-node-over kn) 'none))
                      (knot-knot-nodes k))))

(define (knot-8? k)
  (equal? 1 (length (knot-knot-nodes k))))

(define (knot-detect-loop k)
  (let ([pnodes (knot-path-nodes k)]
        [knodes (knot-knot-nodes k)])
    (findf (lambda (pns) (equal? (path-node-parent (car pns) knodes) (path-node-parent (cadr pns) knodes)))
           (map list pnodes (cycle-left-1 pnodes)))))

(define (knot-detect-loop-knot-node k)
  (let ((knodes (knot-knot-nodes k))
        (pnodes (knot-path-nodes k)))
        (knot-node-path-nodes (findf 
         (lambda (kn) 
           (or (equal? (path-node-next (knot-node-first-path-node kn) pnodes)
                       (knot-node-second-path-node kn))
               (equal? (path-node-next (knot-node-second-path-node kn) pnodes)
                       (knot-node-first-path-node kn))))
         knodes))))

(define (knot-detect-pattern-2 k)
  (let* ((knodes (knot-knot-nodes k))
         (pnodes (knot-path-nodes k))
         (pns-found (findf (lambda (pns) (path-node-pattern-2? pns pnodes knodes))
                     (map list pnodes (cycle-left-1 pnodes)))))
    (if (not pns-found)
        #f
        (path-node-pattern-2? pns-found pnodes knodes))))


(define (path-node-pattern-2? pn1pn2 pns kns)
  (let* ((pn1 (car pn1pn2))
         (pn2 (cadr pn1pn2))
         (kn1 (path-node-parent pn1 kns))
         (kn2 (path-node-parent pn2 kns))
         (pn11 (knot-node-first-path-node kn1))
         (pn12 (knot-node-second-path-node kn1))
         (pn21 (knot-node-first-path-node kn2))
         (pn22 (knot-node-second-path-node kn2))
         (kn1-over (knot-node-over kn1))
         (kn2-over (knot-node-over kn2)))
    (if (and (not (equal? kn1-over 'none))
             (not (equal? kn2-over 'none))
             (equal? (equal? kn1-over pn1) 
                     (equal? kn2-over pn2))
             ;TODO: find and elegant way to use a combinatorics function
             ; maybe all these tests are not usefull
             (or (and (equal? (path-node-next pn11 pns) pn21)
                      (equal? (path-node-next pn12 pns) pn22))
                 (and (equal? (path-node-next pn11 pns) pn21)
                      (equal? (path-node-next pn22 pns) pn12))
                 (and (equal? (path-node-next pn21 pns) pn11)
                      (equal? (path-node-next pn12 pns) pn22))
                 (and (equal? (path-node-next pn21 pns) pn11)
                      (equal? (path-node-next pn22 pns) pn12))
                 (and (equal? (path-node-next pn11 pns) pn22)
                      (equal? (path-node-next pn12 pns) pn21))
                 (and (equal? (path-node-next pn11 pns) pn22)
                      (equal? (path-node-next pn21 pns) pn12))
                 (and (equal? (path-node-next pn22 pns) pn11)
                      (equal? (path-node-next pn12 pns) pn21))
                 (and (equal? (path-node-next pn22 pns) pn11)
                      (equal? (path-node-next pn21 pns) pn12))))
        (list pn11 pn21 pn12 pn22)
        #f)))

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

;(define (knot-copy k)
;  (let1 res (apply make-naked-knot (map path-node-z (knot-path-nodes k)))
;        (map knot-node-copy-over
;             (knot-knot-nodes res)
;             (knot-knot-nodes k))
;        res))

(define (knot-copy k)
  (let* ((pns (map (λ (pn) (struct-copy path-node pn))
                   (knot-path-nodes k)))
         (al-pns (map cons (knot-path-nodes k) pns))
         (f-pns (λ (pn) (cdr (assoc pn (cons '(none . none) al-pns)))))
         (kns (map (λ (kn) (struct-copy knot-node kn 
                                        [first-path-node (f-pns (knot-node-first-path-node kn))]
                                        [second-path-node (f-pns (knot-node-second-path-node kn))]
                                        [over (f-pns (knot-node-over kn))]))
                   (knot-knot-nodes k)))
         (al-kns (map cons (knot-knot-nodes k) kns)))
;    (map (λ (pn-pair) (set-path-node-parent! (cdr pn-pair) (cdr (assoc (path-node-parent (car pn-pair)) al-kns))))
;         al-pns)
    (knot kns pns '())))
                                        
        

(define (knot-play-first k kn)
  (knot-play k kn knot-node-first-path-node))
         

(define (knot-play-second k kn)
  (knot-play k kn knot-node-second-path-node))

(define (knot-play k kn f)
  (let* ((res (knot-copy k))
         (z (knot-node-z kn))
         (res-kn (knot-nearest-node (real-part z) (imag-part z) res)))
    (set-knot-node-over! res-kn (f res-kn))
    res))
         

(define (knot-0? k)
  (if (knot-8? k)
      #t
      (let1 p1 (knot-detect-loop k)
            (if p1
                (knot-0? (knot-remove-pattern k p1))
                (let1 p2 (knot-detect-pattern-2 k)
                      (if p2
                          (knot-0? (knot-remove-pattern k p2))
                          #f))))))

(define (knot-trace k)
  (let1 kns (knot-knot-nodes k)
        (map
         (λ (pn)
           (let* ([kn (path-node-parent pn kns)]
                  [ikn (path-node-parent-index pn kns)]
                  [over (knot-node-over kn)])
             (cond ((equal? 'none over) ikn)
                   ((equal? pn over) (cons ikn #t))
                   (else (cons ikn #f)))))
         (knot-path-nodes k))))


;(define z1 100+200i)
;(define z2 200+100i)
;(define z3 200+300i)
;(define z4 300+200i)
;(define z5 400+100i)
;(define z6 400+300i)
;(define z7 500+200i)

;(define (knot-fill-chords! k) 
;  (let* ((pnodes (knot-path-nodes k))
;         (zs (map path-node-z pnodes))
;         (chords (cycle-map-minus zs)))
;    (map set-path-node-chord-right! pnodes chords)
;    (map set-path-node-chord-left! pnodes (cycle-right-1 chords))))





(define *knot* 
  (make-knot '(100+200i 200+100i 200+300i 300+200i 400+100i 400+300i 500+200i)
             '(0 1 4 6 5 3 1 0 2 5 6 4 3 2)))

(define (make-shadow-7-4)
  (make-knot 
   '(100+200i 
     200+100i 
     200+300i 
     300+200i 
     400+100i 
     400+300i 
     500+200i)
   '(0 1 4 6 5 3 1 0 2 5 6 4 3 2)))

(define (make-shadow-trefoil)
  (make-knot 
   (list
    200+100i 
    400+100i 
    (+ 300+100i (* 1/2 (sqrt 3) 0+200i)))
   '(0 1 2 0 1 2)))

(define (make-shadow-4)
  (make-knot 
   '(200+200i 
     400+200i
     300+258i
     300+373i)
   '(0 1 3 2 1 0 2 3)))

(let* ((k (make-shadow-4))
       (kns (knot-knot-nodes k))
       (kn1 (car kns))
       (kn2 (cadr kns)))
  (set-knot-node-over! kn1 (knot-node-first-path-node kn1))
  (set-knot-node-over! kn2 (knot-node-first-path-node kn2))
  (check-not-false
   (knot-detect-pattern-2 k)))

(let* ([k (make-shadow-7-4)]
       [kns (knot-knot-nodes k)]
       [pns (knot-path-nodes k)]
       [kn0 (findf (lambda (kn) (equal? 100+200i (knot-node-z kn))) kns)]
       [pn01 (knot-node-first-path-node kn0)]
       [pn02 (knot-node-second-path-node kn0)]
       [pn11 (path-node-next pn01 pns)]
       [kn1 (path-node-parent pn11 kns)])
       
  (set-knot-node-over! kn0 pn01)
  (set-knot-node-over! kn1 pn11)
  (check-not-false
   (knot-detect-pattern-2 k)))

(define xknotting-results (make-hash))
(define (knot-xknotting? k complement)
  (let1 args (list (knot-trace k) complement)
        (when (not (hash-has-key? xknotting-results args))
          (hash-set! xknotting-results args (knot-xknotting?-raw k complement)))
        (hash-ref xknotting-results args)))

(define (knot-xknotting?-raw k complement)
  (let* ((kns (filter (lambda (kn) (equal? 'none (knot-node-over kn)))
                      (knot-knot-nodes k)))
         (n-f-ks (filter (lambda (kn-f-k) (not (complement (caddr kn-f-k))))
                          (append-map
                           (lambda (ff)
                             (map (lambda (kn)
                                    (list (list-index (curry equal? kn) (knot-knot-nodes k))
                                          ff (knot-play k kn ff)))
                                  kns))
                           (list knot-node-first-path-node knot-node-second-path-node)))))
    (if (null? n-f-ks)
        #f
        (map (cut take <> 2) n-f-ks))))
  

(define (knot-knotting? k)
  (if (knot-complete? k)
      (not (knot-0? k))
      (knot-xknotting? k knot-unknotting?)))

(define (knot-unknotting? k)
  (if (knot-complete? k)
      (knot-0? k)
      (knot-xknotting? k knot-knotting?)))
                               

(let* ((k0 (make-shadow-trefoil))
       (k1 (knot-play k0 (car (knot-knot-nodes k0)) knot-node-first-path-node))
       (kn1 (findf knot-node-none-over? (knot-knot-nodes k1)))
       (k2 (knot-play k1 kn1 knot-node-first-path-node))
       (kn2 (findf knot-node-none-over? (knot-knot-nodes k2)))
       (k3 (knot-play k2 kn2 knot-node-first-path-node)))
  (check-equal?
   #t
   (knot-unknotting? k3))
  (check-equal?
   #f
   (knot-knotting? k3))
  (check-equal?
   2
   (length (knot-unknotting? k2)))
  (check-equal?
   #f
   (knot-knotting? k0))
  )