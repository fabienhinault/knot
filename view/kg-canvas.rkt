#lang racket


(require racket/gui)
(require racket/draw)
(require "../syntaxes.rkt")
(require "../knot.rkt")
(require "../utils.rkt")

(require "../game.rkt")
(require "../knot.rkt")
(require "../knot-node.rkt")
(require "../path-node.rkt")


(provide kg-canvas%)


(define kg-canvas%
  (class canvas%
    (init agame)
    (super-new)
    (field [game agame]
           [mknot (game-knot agame)]
           [knode '()]
           [pnode '()]
           [circle '()])
    (add-game-play-observer (lambda () (send this refresh)))
    
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
             ;(send this refresh)
             )]
          )))
    
    (define/public (update!)
      (set-game-solver! global-game (lambda (k) (send this solve)))
      (set! circle '())
      (set! game global-game)
      (set! mknot (game-knot global-game))
      (send this refresh))
    
    (add-set-global-game-observer (λ () (send this update!)))
    
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