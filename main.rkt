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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; utils




(define (random-list-ref l)
  (let1 len (length l)
        (list-ref l (random len))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        (let* ([sol (random-list-ref sols)]
               [knode (list-ref (knot-knot-nodes k) (car sol))])
          (game-play g knode ((cadr sol) knode)))
        (random-computer-play g))))

(define (knotter-play g)
  (x-knotter-play g knot-knotting?))

(define (unknotter-play g)
  (x-knotter-play g knot-unknotting?))




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

(define (human-player-play k) '())

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
                              (New_game_etc . "New game...")
                              (Language . "Language")
                              (English . "English")
                              (French . "French")))
(declare-bundle! '(knot fr) '((Game . "Jeu")
                              (New_game . "Nouvelle partie")
                              (New_game_etc . "Nouvelle partie...")
                              (Language . "Langue")
                              (English . "Anglais")
                              (French . "Français")))

(define (new-game player1 player2 make-shadow canvas)
  (let1 g (game (make-shadow) 
                player1 
                player2 
                player1 
                (lambda (k) (send canvas solve)))
        ((class-field-mutator kg-canvas% circle) canvas '())
        ((class-field-mutator kg-canvas% game) canvas g)
        ((class-field-mutator kg-canvas% mknot) canvas (game-knot g))
        (send canvas refresh)
        (game-start g)
        ))

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
         [item-new-game 
          (new menu-item%
               [label (localized-template 'knot 'New_game)]
               [parent menu-game]
               [callback 
                (lambda (mi ce)
                  (new-game player1 player2 make-shadow canvas))])]
         [item-new-game-etc
          (new menu-item%
               [label (localized-template 'knot 'New_game_etc)]
               [parent menu-game]
               [callback 
                (lambda (mi ce)
                  (get-choices-from-user "toto" "le message" '("choix1" "choix2")))])]
         )
    (set-game-solver! g (lambda (k) (send canvas solve)))
    (game-start g)
    (send frame show #t)
    frame))

(define frame (start human-player-play human-player-play make-shadow-7-4 'fr))

;(start human-player-play knotter-play make-shadow-7-4 'fr))
;(start human-player-play unknotter-play make-shadow-7-4 'fr))