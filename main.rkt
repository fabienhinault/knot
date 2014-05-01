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
(require "game.rkt")


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



;
;(define (random-list-ref l)
;  (let1 len (length l)
;        (list-ref l (random len))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;

;(struct game
;  (knot
;   player1
;   player2
;   current-player
;   solver)
;  #:mutable
;  #:transparent)
;
;(define (game-start g)
;  ((game-current-player g) g))
;
;(let1 observers
;      '()
;      (define (game-play g knode pnode)
;        (set-knot-node-over! knode pnode)
;        (if (knot-complete? (game-knot g))
;            ((game-solver g) (game-knot g))
;            (begin
;              (change-game-current-player! g)
;              ((game-current-player g) g)))
;        (map apply observers)))
;
;(define (change-game-current-player! g)
;  (set-game-current-player! 
;   g 
;   (if (equal? (game-player1 g) (game-current-player g))
;       (game-player2 g)
;       (game-player1 g))))
;
;(define (x-knotter-play g p?)
;  (let* ((k (game-knot g))
;         (sols (p? k)))
;    (if sols
;        (let* ([sol (random-list-ref sols)]
;               [knode (list-ref (knot-knot-nodes k) (car sol))])
;          (game-play g knode ((cadr sol) knode)))
;        (random-computer-play g))))
;
;(define (knotter-play g)
;  (x-knotter-play g knot-knotting?))
;
;(define (unknotter-play g)
;  (x-knotter-play g knot-unknotting?))
;
;
;
;
;(define (dumb-computer-play g)
;  (let* ((k (game-knot g))
;         (knode (findf (lambda (kn) (equal? (knot-node-over kn) 'none))
;                       (knot-knot-nodes k))))
;    (when knode
;      (game-play g knode (knot-node-first-path-node knode)))))
;
;(define (random-computer-play g)
;  (let* ((k (game-knot g))
;         (knode
;          (random-list-ref 
;           (filter 
;            (lambda (kn) (equal? (knot-node-over kn) 'none))
;            (knot-knot-nodes k)))))
;    (when knode
;      (game-play
;       g
;       knode 
;       ((if (equal? 0 (random 2))
;            knot-node-first-path-node
;            knot-node-second-path-node)
;        knode)))))
;
;(define (human-player-play k) '())

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
                              (French . "French")
                              (Against_the_computer . "Against the computer")
                              (2_players . "2 players")
                              (Computer_starts . "Computer starts")
                              (You_start . "You start")
                              (You_knot . "You knot")
                              (You_do_NOT_knot . "You do not knot")
                              
                              )
                 )
(declare-bundle! '(knot fr) '((Game . "Jeu")
                              (New_game . "Nouvelle partie")
                              (New_game_etc . "Nouvelle partie...")
                              (Language . "Langue")
                              (English . "Anglais")
                              (French . "Français")
                              (Against_the_computer . "Contre l'ordinateur")
                              (2_players . "2 joueurs")
                              (Computer_starts . "L'ordinateur commence")
                              (You_start . "Vous commencez")
                              
                              (You_knot . "Vous nouez")
                              (You_do_NOT_knot . "Vous dénouez")
                              
                              
                              )
                 )

(define (new-game make-shadow
                  canvas 
                  #:old-game [old-game '()] 
                  #:player1 [player1 (game-player1 old-game)] 
                  #:player2 [player2 (game-player2 old-game)])
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
        g
        ))

(define (get-choice-tree-from-user title message choice-trees)
  (when (not (null? choice-trees))
    (let* ([current-tree (car choice-trees)]
           [current-choice
            (get-choices-from-user (localized-template 'knot title) message
                                   (map (λ (m) (localized-template 'knot (car m))) current-tree))])
      ((cadr (list-ref current-tree (car current-choice))))
      (get-choice-tree-from-user title message (cddr (list-ref current-tree (car current-choice))))
      (get-choice-tree-from-user title message (cdr choice-trees)))))

(define (game-status g)
  (let1 game-status-messages
        (list (list human-player-play human-player-play '2_players)
              (list knotter-play human-player-play 'Against_computer 'You_do_NOT_knot)
              (list human-player-play knotter-play 'Against_computer 'You_do_NOT_knot 'You_start)
              (list unknotter-play human-player-play 'Against_computer 'You_knot)
              (list human-player-play unknotter-play 'Against_computer 'You_knot 'You_start))
        (cddr (findf (λ (l) (and (equal? (car l) (game-player1 g))
                                 (equal? (cadr l) (game-player2 g))))
                     game-status-messages))))

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
                  (set! g (new-game make-shadow canvas #:old-game g)))])]
         [dialog-new-game
          (new dialog% 
               [label (localized-template 'knot 'New_game_etc)]
               [parent frame])]
         [radio-nb-players
          (new radio-box%
               [label ""]
               [choices (list (localized-template 'knot 'Against_the_computer)
                              (localized-template 'knot '2_players))]
               [parent dialog-new-game])]
         [fplayer1 '()]
         [fplayer2 '()]
         [player-computer '()]
         [new-game-choice-tree
          (list (list (list 'Against_the_computer 
                            (λ () '()) 
                            (list (list 'Computer_starts 
                                        (λ () 
                                          (set! fplayer1 (λ () player-computer))
                                          (set! fplayer2 (λ () human-player-play))))
                                  (list 'You_start
                                        (λ () 
                                          (set! fplayer1 (λ () human-player-play))
                                          (set! fplayer2 (λ () player-computer)))))
                            (list (list 'You_knot
                                        (λ ()
                                          (set! player-computer unknotter-play)
                                          (set! g (new-game make-shadow canvas #:player1 (fplayer1) #:player2 (fplayer2)))))
                                  (list 'You_do_NOT_knot
                                        (λ ()
                                          (set! player-computer knotter-play)
                                          (set! g (new-game make-shadow canvas #:player1 (fplayer1) #:player2 (fplayer2)))))))
                      (list '2_players
                            (λ () (set! g (new-game make-shadow canvas #:player1 human-player-play #:player2 human-player-play))))))]
         [item-new-game-etc 
          (new menu-item%
               [label (localized-template 'knot 'New_game_etc)]
               [parent menu-game]
               [callback 
                (lambda (mi ce)
                  (get-choice-tree-from-user 'New_game_etc "" new-game-choice-tree))])]
         [game-type-label
          (map (λ (sym) (localized-template 'knot sym)) (game-status g))]
         [status-bar
          (new message%
               [parent frame]
               [label (foldl (λ (str1 str2) 
                               (string-append 
                                str1 " | " str2))
                             (car game-type-label)
                             (cdr game-type-label))])]
         
         )
    (set-game-solver! g (lambda (k) (send canvas solve)))
    (game-start g)
    (send frame show #t)
    frame))

(define frame (start human-player-play human-player-play make-shadow-7-4 'fr))

;(start human-player-play knotter-play make-shadow-7-4 'fr)
;(start human-player-play unknotter-play make-shadow-7-4 'fr)