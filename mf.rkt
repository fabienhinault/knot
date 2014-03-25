#lang racket

(require racket/gui)
(require racket/draw)
(require rackunit)
;(require srfi/1) ; list-index
(require srfi/26) ; cut cute
(require srfi/29) ; l10n
(require "syntaxes.rkt")
(require "utils.rkt")
(require "path-node.rkt")
(require "knot-node.rkt")
(require "knot.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;syntaxes

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

; replace define with a memoized version
(define-syntax define-memoized
  (syntax-rules ()
    [(_ (f args ...) bodies ...)
     (define f
       ; store the cache as a hash of args => result
       (let ([results (make-hash)])
         ; need to do this to capture both the names and the values
         (lambda (args ...)
           ((lambda vals
              ; if we haven't calculated it before, do so now
              (when (not (hash-has-key? results vals))
                (hash-set! results vals (begin bodies ...)))
              ; return the cached result
              (hash-ref results vals))
            args ...))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; utils




;(define (make-cycle l)
;  (let* ([ph (make-placeholder #f)]
;         [x (append l (list ph))])
;    (placeholder-set! ph x)
;    (make-reader-graph x)))


(define (random-list-ref l)
  (let1 len (length l)
        (list-ref l (random len))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; I don't know which one is more efficient, this one or
;  (angle (/ chord2 chord1)))

; model
; knot

;(struct path-node 
;  (z 
;   chord-left ;  vector from previous node to this one
;   chord-right ; vector from this node to next one
;   theta ; angle from chord-right to (- control-right z)
;   phi ; angle from (- z control-left) to (chord-left)
;   ;(equal? 0 (+ phi theta psi))
;   ; with psi turn angle from chord-left to chord-right
;   control-left 
;   control-right)
;   ;parent)
;  #:mutable
;  #:transparent)
;
;(define (path-node-angle pn)
;  (let1 chord (path-node-chord-right pn)
;        (if (and (not (null? chord)) (not (equal? 0 chord)))
;            (+ (angle chord) 
;               (path-node-theta pn))
;            (angle (- (path-node-control-right pn) (path-node-z pn))))))
;
;(define (set-path-node-angle! pn angle-val)
;  (set-path-node-theta! 
;   pn 
;   (- angle-val 
;      (angle (path-node-chord-right pn)))))
;
;(define (path-node-over? pn kns)
;  (equal? pn (knot-node-over (path-node-parent pn kns))))
;
;(define (add-path-node-theta! pn d-theta)
;  (set-path-node-theta! pn (+ (path-node-theta pn) d-theta)))
;
;(define (path-node-next pn-cur pns)
;  (let1 pns-cur (member pn-cur pns)
;        (if (not (null? (cdr pns-cur)))
;            (cadr pns-cur)
;            (car pns))))
;
;(define (path-node-copy-angle pn-to pn-from)
;  (if (equal? 0 (path-node-chord-right pn-to))
;      (set-path-node-control-right! 
;       pn-to 
;       (path-node-control-right pn-from))
;      (set-path-node-angle! pn-to (path-node-angle pn-from)))
;  (if (equal? 0 (path-node-chord-left pn-to))
;      (set-path-node-control-left! 
;       pn-to 
;       (path-node-control-left pn-from))
;      (set-path-node-phi! 
;       pn-to
;       (- (angle (path-node-chord-left pn-to))
;          (path-node-angle pn-from) 
;          ))))
;
;(define (fill-path-node-control-right! pnode-k pnode-k+1)
;  (set-path-node-control-right! 
;   pnode-k
;   (right-control-point (path-node-z pnode-k)
;                        (path-node-chord-right pnode-k)
;                        (path-node-theta pnode-k)
;                        (path-node-phi pnode-k+1))))
;
;(define (fill-path-node-control-left! pnode-k pnode-k+1)
;  (set-path-node-control-left! 
;   pnode-k+1
;   (left-control-point (path-node-z pnode-k+1)
;                       (path-node-chord-left pnode-k+1)
;                       (path-node-theta pnode-k)
;                       (path-node-phi pnode-k+1))))
;
;(define (fill-path-node-control-points! pnode-k pnode-k+1)
;  (let ((chord-left (path-node-chord-left pnode-k+1))
;        (chord-right (path-node-chord-right pnode-k)))
;    (when (not (equal? 0 chord-left))
;      (fill-path-node-control-left! pnode-k pnode-k+1))
;    (when (not (equal? 0 chord-right))
;      (fill-path-node-control-right! pnode-k pnode-k+1))))
;
;
;(define (turnAngle chord1 chord2)
;  (if (or (equal? 0 chord1) (equal? 0 chord2))
;      '()
;      (-mod (* 2 pi) (angle chord2) (angle chord1))))
;; I don't know which one is more efficient, this one or
;;  (angle (/ chord2 chord1)))
;
;(define (path-node-control-radius-average pn)
;  (/ (+ (magnitude (- (path-node-control-left pn) (path-node-z pn)))
;        (magnitude (- (path-node-control-right pn) (path-node-z pn))))
;     2))
;
;(define (path-node-parent pn kns)
;  (findf (cut knot-node-parent? <> pn)
;         kns))
;
;(define (path-node-parent-index pn kns)
;  (list-index (cut knot-node-parent? <> pn) kns))


;;;
;
;(struct knot-node (z first-path-node second-path-node over) #:mutable #:transparent)
;
;(define (knot-node-path-nodes kn) 
;  (list (knot-node-first-path-node kn) (knot-node-second-path-node kn)))
;;(define (knot-node-other-path-node kn pn)
;;  (findf (lambda (x) (not (equal? x pn)))
;;         (knot-node-path-nodes kn)))
;
;(let* ((pns '(1 2 3))
;       (kn (knot-node 'z (car pns) (cadr pns) 'none)))
;  (check-equal?
;   (knot-node-path-nodes kn) '(1 2)))
;;  (check-equal?
;;   (knot-node-other-path-node kn 2) 1))
;
;(define (knot-node-copy-over kn-to kn-from)
;  (set-knot-node-over!
;   kn-to 
;   (if (equal? 'none  (knot-node-over kn-from))
;       'none
;       (minf (knot-node-path-nodes kn-to)
;             (lambda (pn) 
;               (abs (- (path-node-angle pn)
;                       (path-node-angle (knot-node-over kn-from)))))))))
;
;
;(define (tweak-angles angle1 angle2)
;; tweaks 2 angles so that they cross orthogonally
;  (let* ((offset (- angle2 angle1))
;         (target-offset (+ (/ pi 2) 
;                           (* (floor (/ offset pi)) 
;                              pi))))
;    (/ (- offset target-offset) 2)))
;
;(check-equal?
; (+ -1.4 (tweak-angles -1.4 1.4))
; (- (/ pi 4)))
;
;(check-equal?
; (+ -1.450515964828205 (tweak-angles -1.450515964828205 1.450515964828205))
; (- (/ pi 4)))
;
;(check-equal?
; (+ 1.4 (tweak-angles 1.4 -1.4))
; (/ pi 4))
;
;(check-equal?
; (< (tweak-angles -0.3345558925337896 3.288158208922769)  0 )
; #true)
;
;(define (knot-node-tweak-thetas! kn)
;  (let* ((pn1 (knot-node-first-path-node kn))
;         (pn2 (knot-node-second-path-node kn))
;         (tweak (tweak-angles  (path-node-angle pn1) (path-node-angle pn2))))
;    (add-path-node-theta! pn1 (+ tweak))
;    (add-path-node-theta! pn2 (- tweak))))
;
;(define (knot-node-angle-path-node kn angle)
;  (let* ((pn1 (knot-node-first-path-node kn))
;         (angle1 (path-node-angle pn1))
;         (pn2 (knot-node-second-path-node kn))
;         (angle2 (path-node-angle pn2)))
;    (if (< (abs (cos (- angle angle1)))
;           (abs (cos (- angle angle2))))
;        pn2
;        pn1)))
;
;(define (knot-node-none-over? kn)
;  (equal? 'none (knot-node-over kn)))
;
;(define (knot-node-parent? kn pn)
;  (or (equal? pn (knot-node-first-path-node kn))
;      (equal? pn (knot-node-second-path-node kn))))

;;;;
;;knot
;
;;knot drawing math utils
;;mf276
;(define (knot-matrix chords)
;  (define n (length chords))
;  (define (prev i)
;    (modulo (sub1 i) n))
;  (define (next i)
;    (modulo (add1 i) n))
;  (define (A i j)
;    (if (not (eq? j (prev i)))
;        0
;        (/ 1 (magnitude (list-ref chords j)))))
;  (define (B+C i j)
;    (if (not (eq? i j))
;        0
;        (+ (/ 2 (magnitude (list-ref chords (prev i))))
;           (/ 2 (magnitude (list-ref chords i))))))
;  (define (D i j)
;    (if (not (eq? j (next i)))
;        0
;        (/ 1 (magnitude (list-ref chords i)))))
;  (build-matrix 
;     n n
;     (lambda (i j) (+ (A i j) (B+C i j) (D i j)))))
;
;(define (knot-right-vector chords turnAngles)
;  (define n (length chords))
;  (define (prev i)
;    (modulo (sub1 i) n))
;  (define (next i)
;    (modulo (add1 i) n))
;  (build-matrix 
;   (length chords) 1
;   (lambda (i j) 
;     (- 0
;        (* (/ 2 (magnitude (list-ref chords (prev i)))) 
;           (list-ref turnAngles i))
;        (* (/ 1 (magnitude (list-ref chords i))) 
;           (list-ref turnAngles (next i)))))))
;
;
;;knot graphic utils
;(define z-path%
;  (class dc-path%
;    (define (z-curve-to z1 z2 z3)
;      (send this curve-to
;            (real-part z1)
;            (imag-part z1)
;            (real-part z2)
;            (imag-part z2)
;            (real-part z3)
;            (imag-part z3)))
;    (define (z-move-to z)
;      (send this move-to
;            (real-part z)
;            (imag-part z)))
;    (public z-curve-to)
;    (public z-move-to)
;    (super-new)))
;
;(struct knot (knot-nodes path-nodes path)  #:mutable #:transparent)
;
;(define (make-naked-knot . points)
;  (define (make-knot-node pnodes)
;    (cond ((null? pnodes) '())
;          (else
;           (let* ((pnode (car pnodes))
;                  (z (path-node-z pnode))
;                  (pnodes2 (memf (lambda (pn) (equal? z (path-node-z pn))) (cdr pnodes)))
;                  (knode (knot-node z pnode (car pnodes2) 'none)))
;;             (set-path-node-parent! pnode knode)
;;             (set-path-node-parent! (car pnodes2) knode)
;             knode))))
;  
;  
;  (define (make-knot-nodes pnodes knodes)
;    (cond ((null? pnodes) knodes)
;          ((findf (lambda (kn) (member (car pnodes) (knot-node-path-nodes kn)))
;                  knodes)
;           (make-knot-nodes (cdr pnodes) knodes))
;          (else
;           (make-knot-nodes (cdr pnodes) (cons (make-knot-node pnodes) knodes)))))
;  
;  (let* ((path-nodes (map 
;                     (lambda (z) (path-node z '() '() '() '() '() '() ))
;                     points)))
;    (knot (make-knot-nodes path-nodes '()) path-nodes '())))
;
;
;; many things borrowed from
;; http://hackage.haskell.org/package/cubicbezier-0.2.0/docs/Geom2D-CubicBezier-MetaPath.html
;; ftp://db.stanford.edu/pub/cstr/reports/cs/tr/85/1047
;; http://texdoc.net/texmf-dist/doc/generic/knuth/mf/mf.pdf
;
;(define (make-knot zs order)
;  (let* ((points (map (lambda (i) (list-ref zs i)) order))
;         (k (apply make-naked-knot points))
;         (chords (cycle-map-minus points))
;         (turnAngles (map turnAngle (cycle-right-1 chords) chords))
;         (orig-thetas (matrix->list 
;                       (matrix-solve 
;                        (knot-matrix chords)
;                        (knot-right-vector chords turnAngles))))
;         (pnodes (knot-path-nodes k)))
;    (map set-path-node-chord-right! pnodes chords)
;    (map set-path-node-chord-left! pnodes (cycle-right-1 chords))
;    (map set-path-node-theta! pnodes orig-thetas)
;    (map knot-node-tweak-thetas! (knot-knot-nodes k))
;    (knot-fill-phis! k turnAngles)
;    (map fill-path-node-control-points! pnodes (cycle-left-1 pnodes))
;    (knot-fill-path! k)
;    k))
;
;(define (knot-fill-phis! k turnAngles)
;  (map 
;   (lambda (pnode turnAngle)
;     (set-path-node-phi! pnode (- 0 (path-node-theta pnode) turnAngle)))
;   (knot-path-nodes k)
;   turnAngles))
;
;(define (knot-fill-path! k)
;  (let ((z-path (new z-path%))
;        (pnodes (knot-path-nodes k)))
;    (send z-path z-move-to (path-node-z (car (knot-path-nodes k))))
;    (for ([pnode-k pnodes]
;          [pnode-k+1 (left-cycle-1 pnodes)])
;      (send z-path z-curve-to (path-node-control-right pnode-k)
;            (path-node-control-left pnode-k+1)
;            (path-node-z pnode-k+1)))
;    (set-knot-path! k z-path)))
;
;
;(define (knot-draw k dc)
;  (let ((pen (send dc get-pen)))
;    (send dc set-pen "black" 5 'solid)
;    (send dc draw-path (knot-path k))
;    (for ([knode (knot-knot-nodes k)])
;      (when (not (equal? 'none (knot-node-over knode)))
;        (let* ((pnode (knot-node-over knode))
;               (z (path-node-z pnode))
;               (z-angle (make-polar 1 (path-node-angle pnode))))
;          (send dc set-pen "white" 15 'solid)
;          (draw-z-line dc z (* 5 z-angle))
;          (send dc set-pen "black" 5 'solid)
;          (draw-z-line dc z (* 10 z-angle)))))
;    (send dc set-pen pen)))
;
;(define (knot-nearest-node x y aknot)
;  (let ((nearest (minf
;                  (knot-knot-nodes aknot)
;                  (lambda (k-node) (magnitude (- (knot-node-z k-node) (make-rectangular x y)))))))
;    (if (equal? (knot-node-over nearest) 'none)
;        nearest
;        '())))
;
;(let* ((k  (make-knot '(100+200i 200+100i 200+300i 300+200i 400+100i 400+300i 500+200i)
;                       '(0 1 4 6 5 3 1 0 2 5 6 4 3 2)))
;       (kn100+200i (findf (lambda (kn) (equal? (knot-node-z kn) 100+200i)) (knot-knot-nodes k))))
;  (check-equal? 
;   (knot-nearest-node 
;    50 200 
;    k)
;   kn100+200i)
;  (set-knot-node-over! kn100+200i (knot-node-first-path-node kn100+200i))
;  (check-equal? 
;   (knot-nearest-node 
;    50 200 
;    k)
;   '()))
;
;(define (knot-complete? k)
;  (equal? #f (findf (lambda (kn) (equal? (knot-node-over kn) 'none))
;                      (knot-knot-nodes k))))
;
;(define (knot-8? k)
;  (equal? 1 (length (knot-knot-nodes k))))
;
;(define (knot-detect-loop k)
;  (let ([pnodes (knot-path-nodes k)]
;        [knodes (knot-knot-nodes k)])
;    (findf (lambda (pns) (equal? (path-node-parent (car pns) knodes) (path-node-parent (cadr pns) knodes)))
;           (map list pnodes (cycle-left-1 pnodes)))))
;
;(define (knot-detect-loop-knot-node k)
;  (let ((knodes (knot-knot-nodes k))
;        (pnodes (knot-path-nodes k)))
;        (knot-node-path-nodes (findf 
;         (lambda (kn) 
;           (or (equal? (path-node-next (knot-node-first-path-node kn) pnodes)
;                       (knot-node-second-path-node kn))
;               (equal? (path-node-next (knot-node-second-path-node kn) pnodes)
;                       (knot-node-first-path-node kn))))
;         knodes))))
;
;(define (knot-detect-pattern-2 k)
;  (let* ((knodes (knot-knot-nodes k))
;         (pnodes (knot-path-nodes k))
;         (pns-found (findf (lambda (pns) (path-node-pattern-2? pns pnodes knodes))
;                     (map list pnodes (cycle-left-1 pnodes)))))
;    (if (not pns-found)
;        #f
;        (path-node-pattern-2? pns-found pnodes knodes))))
;
;
;(define (path-node-pattern-2? pn1pn2 pns kns)
;  (let* ((pn1 (car pn1pn2))
;         (pn2 (cadr pn1pn2))
;         (kn1 (path-node-parent pn1 kns))
;         (kn2 (path-node-parent pn2 kns))
;         (pn11 (knot-node-first-path-node kn1))
;         (pn12 (knot-node-second-path-node kn1))
;         (pn21 (knot-node-first-path-node kn2))
;         (pn22 (knot-node-second-path-node kn2))
;         (kn1-over (knot-node-over kn1))
;         (kn2-over (knot-node-over kn2)))
;    (if (and (not (equal? kn1-over 'none))
;             (not (equal? kn2-over 'none))
;             (equal? (equal? kn1-over pn1) 
;                     (equal? kn2-over pn2))
;             ;TODO: find and elegant way to use a combinatorics function
;             ; maybe all these tests are not usefull
;             (or (and (equal? (path-node-next pn11 pns) pn21)
;                      (equal? (path-node-next pn12 pns) pn22))
;                 (and (equal? (path-node-next pn11 pns) pn21)
;                      (equal? (path-node-next pn22 pns) pn12))
;                 (and (equal? (path-node-next pn21 pns) pn11)
;                      (equal? (path-node-next pn12 pns) pn22))
;                 (and (equal? (path-node-next pn21 pns) pn11)
;                      (equal? (path-node-next pn22 pns) pn12))
;                 (and (equal? (path-node-next pn11 pns) pn22)
;                      (equal? (path-node-next pn12 pns) pn21))
;                 (and (equal? (path-node-next pn11 pns) pn22)
;                      (equal? (path-node-next pn21 pns) pn12))
;                 (and (equal? (path-node-next pn22 pns) pn11)
;                      (equal? (path-node-next pn12 pns) pn21))
;                 (and (equal? (path-node-next pn22 pns) pn11)
;                      (equal? (path-node-next pn21 pns) pn12))))
;        (list pn11 pn21 pn12 pn22)
;        #f)))
;
;(define (knot-remove-pattern k pattern)
;  (let* ((pnodes (filter (lambda (pn) (not (member pn pattern)))
;                         (knot-path-nodes k)))
;         (knodes (filter (lambda (kn) (not (member (knot-node-over kn) pattern)))
;                         (knot-knot-nodes k)))
;         (points (map path-node-z pnodes))
;         (chords (cycle-map-minus points))
;         (res (apply make-naked-knot points))
;         (res-pnodes (knot-path-nodes res)))
;    (map set-path-node-chord-right! res-pnodes chords)
;    (map set-path-node-chord-left! res-pnodes (cycle-right-1 chords))
;    (map path-node-copy-angle res-pnodes pnodes)
;    (map knot-node-copy-over
;         (knot-knot-nodes res)
;         knodes)
;    (map fill-path-node-control-points! res-pnodes (cycle-left-1 res-pnodes))
;    (knot-fill-path! res)
;    res))
;
;;(define (knot-copy k)
;;  (let1 res (apply make-naked-knot (map path-node-z (knot-path-nodes k)))
;;        (map knot-node-copy-over
;;             (knot-knot-nodes res)
;;             (knot-knot-nodes k))
;;        res))
;
;(define (knot-copy k)
;  (let* ((pns (map (λ (pn) (struct-copy path-node pn))
;                   (knot-path-nodes k)))
;         (al-pns (map cons (knot-path-nodes k) pns))
;         (f-pns (λ (pn) (cdr (assoc pn (cons '(none . none) al-pns)))))
;         (kns (map (λ (kn) (struct-copy knot-node kn 
;                                        [first-path-node (f-pns (knot-node-first-path-node kn))]
;                                        [second-path-node (f-pns (knot-node-second-path-node kn))]
;                                        [over (f-pns (knot-node-over kn))]))
;                   (knot-knot-nodes k)))
;         (al-kns (map cons (knot-knot-nodes k) kns)))
;;    (map (λ (pn-pair) (set-path-node-parent! (cdr pn-pair) (cdr (assoc (path-node-parent (car pn-pair)) al-kns))))
;;         al-pns)
;    (knot kns pns '())))
;                                        
;        
;
;(define (knot-play-first k kn)
;  (knot-play k kn knot-node-first-path-node))
;         
;
;(define (knot-play-second k kn)
;  (knot-play k kn knot-node-second-path-node))
;
;(define (knot-play k kn f)
;  (let* ((res (knot-copy k))
;         (z (knot-node-z kn))
;         (res-kn (knot-nearest-node (real-part z) (imag-part z) res)))
;    (set-knot-node-over! res-kn (f res-kn))
;    res))
;         
;
;(define (knot-0? k)
;  (if (knot-8? k)
;      #t
;      (let1 p1 (knot-detect-loop k)
;            (if p1
;                (knot-0? (knot-remove-pattern k p1))
;                (let1 p2 (knot-detect-pattern-2 k)
;                      (if p2
;                          (knot-0? (knot-remove-pattern k p2))
;                          #f))))))
;
;(define (knot-trace k)
;  (let1 kns (knot-knot-nodes k)
;        (map
;         (λ (pn)
;           (let* ([kn (path-node-parent pn kns)]
;                  [ikn (path-node-parent-index pn kns)]
;                  [over (knot-node-over kn)])
;             (cond ((equal? 'none over) ikn)
;                   ((equal? pn over) (cons ikn #t))
;                   (else (cons ikn #f)))))
;         (knot-path-nodes k))))
;
;
;;(define z1 100+200i)
;;(define z2 200+100i)
;;(define z3 200+300i)
;;(define z4 300+200i)
;;(define z5 400+100i)
;;(define z6 400+300i)
;;(define z7 500+200i)
;
;;(define (knot-fill-chords! k) 
;;  (let* ((pnodes (knot-path-nodes k))
;;         (zs (map path-node-z pnodes))
;;         (chords (cycle-map-minus zs)))
;;    (map set-path-node-chord-right! pnodes chords)
;;    (map set-path-node-chord-left! pnodes (cycle-right-1 chords))))
;
;
;
;
;
;(define *knot* 
;  (make-knot '(100+200i 200+100i 200+300i 300+200i 400+100i 400+300i 500+200i)
;             '(0 1 4 6 5 3 1 0 2 5 6 4 3 2)))
;
;(define (make-shadow-7-4)
;  (make-knot 
;   '(100+200i 
;     200+100i 
;     200+300i 
;     300+200i 
;     400+100i 
;     400+300i 
;     500+200i)
;   '(0 1 4 6 5 3 1 0 2 5 6 4 3 2)))
;
;(define (make-shadow-trefoil)
;  (make-knot 
;   (list
;    200+100i 
;    400+100i 
;    (+ 300+100i (* 1/2 (sqrt 3) 0+200i)))
;   '(0 1 2 0 1 2)))
;
;(define (make-shadow-4)
;  (make-knot 
;   '(200+200i 
;     400+200i
;     300+258i
;     300+373i)
;   '(0 1 3 2 1 0 2 3)))
;
;(let* ((k (make-shadow-4))
;       (kns (knot-knot-nodes k))
;       (kn1 (car kns))
;       (kn2 (cadr kns)))
;  (set-knot-node-over! kn1 (knot-node-first-path-node kn1))
;  (set-knot-node-over! kn2 (knot-node-first-path-node kn2))
;  (check-not-false
;   (knot-detect-pattern-2 k)))
;
;(let* ((k (make-shadow-7-4))
;       (kns (knot-knot-nodes k))
;       (kn0 (list-ref kns 0))
;       (kn2 (findf (lambda (kn) (equal? 200+300i (knot-node-z kn))) kns)))
;  (set-knot-node-over! kn0 (knot-node-second-path-node kn0))
;  (set-knot-node-over! kn2 (knot-node-first-path-node kn2))
;  (check-not-false
;   (knot-detect-pattern-2 k)))

;(define (all2lists x y)
;  (list (list x y) (list y x)))
;

;;;

(struct game
  (knot
   player1
   player2
   current-player
   solver)
  #:mutable
  #:transparent)

(define (game-start g)
  ((game-current-player g) g))

(define (game-play g knode pnode)
  (set-knot-node-over! knode pnode)
  (if (knot-complete? (game-knot g))
      ((game-solver g) (game-knot g))
      (begin
        (change-game-current-player! g)
        ((game-current-player g) g))))

(define (change-game-current-player! g)
  (set-game-current-player! 
   g 
   (if (equal? (game-player1 g) (game-current-player g))
       (game-player2 g)
       (game-player1 g))))

(define (x-knotter-play g p?)
  (let* ((k (game-knot g))
         (sols (p? k)))
    (if sols
        (let1 sol (random-list-ref sols)
              (game-play g (car sol) ((cadr sol) (car sol))))
        (random-computer-play g))))

(define (knotter-play g)
  (x-knotter-play g knot-knotting?))

(define (unknotter-play g)
  (x-knotter-play g knot-unknotting?))

;without memoizing
;(time (knot-knotting? (make-shadow-7-4)))
;cpu time: 1576588 real time: 1578695 gc time: 68340

;(time (knot-knotting? (make-shadow-7-4)))
;cpu time: 79824 real time: 79840 gc time: 2092



(define (dumb-computer-play g)
  (let* ((k (game-knot g))
         (knode (findf (lambda (kn) (equal? (knot-node-over kn) 'none))
                      (knot-knot-nodes k))))
    (when knode
      (game-play g knode (knot-node-first-path-node knode)))))

(define (random-computer-play g)
  (let* ((k (game-knot g))
         (knode
          (random-list-ref 
           (filter 
            (lambda (kn) (equal? (knot-node-over kn) 'none))
            (knot-knot-nodes k)))))
   (when knode
      (game-play
       g
       knode 
       ((if (equal? 0 (random 2))
                knot-node-first-path-node
                knot-node-second-path-node)
             knode)))))

(define (2-players-play k) '())

(define (re-play)
  (eval (read)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;graphic view


(define (draw-z-point dc z)
  (send dc draw-point (real-part z) (imag-part z)))




(define kg-canvas%
  (class canvas%
    (init agame)
    (super-new)
    (field [game agame]
           [mknot (game-knot agame)]
           [knode '()]
           [pnode '()]
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
                    (z (make-rectangular x y)))
               (set! knode (knot-nearest-node x y mknot))
               (let1 knode-z (if (not (null? knode)) (knot-node-z knode) '())
                     (if (and (not (null? knode-z)) (not (equal? (- knode-z z) 0)))
                         (set! pnode (knot-node-angle-path-node knode (angle (- z knode-z))))
                         (set! pnode '())))
               (send this refresh)))]
          [(left-up)
           (when (not (null? pnode))
             (game-play game knode pnode)
             (set! pnode '())
             (set! knode '())
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
      (set! pnode '())
      (send this refresh)
      (yield)
      (let* ((p2 (knot-detect-pattern-2 mknot))
             (p1 (knot-detect-loop mknot))
             (knodes (knot-knot-nodes mknot)))
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
                     (let ((kn1 (path-node-parent (car p2) knodes))
                           (kn2 (path-node-parent (cadr p2) knodes)))
                       (w/pen dc "yellow" 20 'solid  
                              (draw-z-point dc (knot-node-z kn1))
                              (draw-z-point dc (knot-node-z kn2))))))
           (set! mknot (knot-remove-pattern mknot p2))
           (send this refresh)
           (send this solve)))))      
    ))

(declare-bundle! '(knot en) '((Game . "Game")
                       (New_game . "New game")
                       (Language . "Language")
                       (English . "English")
                       (French . "French")))
(declare-bundle! '(knot fr) '((Game . "Jeu")
                       (New_game . "Nouvelle partie")
                       (Language . "Langue")
                       (English . "Anglais")
                       (French . "Français")))

(define (start player1 player2 make-shadow lang)
  (current-language lang)
  (let* ((g (game (make-shadow) player1 player2 player1 '()))
         (frame (new frame%
                     [label "To knot or not to knot"]
                     [width 600]
                     [height 450]))
         (menu-bar (new menu-bar%
                      (parent frame)))
         (canvas
          (new kg-canvas%
               [parent frame]
               [agame g]))
         (menu-game (new menu%
              (label (localized-template 'knot 'Game))
              (parent menu-bar)))
         (item-new-game 
          (new menu-item%
               [label (localized-template 'knot 'New_game)]
               [parent menu-game]
               [callback 
                (lambda (mi ce)
                  (let1 g
                        (game (make-shadow) 
                                player1 
                                player2 
                                player1 
                                (lambda (k) (send canvas solve)))
                        ((class-field-mutator kg-canvas% circle) 
                         canvas '())
                        ((class-field-mutator kg-canvas% game) 
                         canvas g)
                        ((class-field-mutator kg-canvas% mknot) 
                         canvas (game-knot g))
                        (send canvas refresh)
                        (game-start g)
                        ))])))
    (set-game-solver! g (lambda (k) (send canvas solve)))
    (game-start g)
    (send frame show #t)
    frame))

(define frame (start 2-players-play 2-players-play make-shadow-7-4 'fr))