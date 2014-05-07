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
(require "view/kg-canvas.rkt")
(require "view/kg-frame.rkt")


(define-syntax-rule (w/brush dc size color body ...)
  (let1 brush (send dc get-pen)
        (send dc set-brush color size 'solid)
        body ...
        (send dc set-brush brush)))



(declare-bundle! '(knot en) '((Game . "Game")
                              (New_game . "New game")
                              (New_game_etc . "New game...")
                              (Language . "Language")
                              (English . "English")
                              (French . "French")
                              (Against_the_computer . "Against the computer")
                              (2_players . "2 players")
                              (Over_the_network . "Over the network")
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
                              (Over_the_network . "En réseau")
                              (Computer_starts . "L'ordinateur commence")
                              (You_start . "Vous commencez")
                              
                              (You_knot . "Vous nouez")
                              (You_do_NOT_knot . "Vous dénouez")
                              
                              
                              )
                 )


(define (get-choice-tree-from-user title message choice-trees)
  (when (not (null? choice-trees))
    (let* ([current-tree (car choice-trees)]
           [current-choice
            (get-choices-from-user (localized-template 'knot title) message
                                   (map (λ (m) (localized-template 'knot (car m))) current-tree))])
      ((cadr (list-ref current-tree (car current-choice))))
      (get-choice-tree-from-user title message (cddr (list-ref current-tree (car current-choice))))
      (get-choice-tree-from-user title message (cdr choice-trees)))))




(define (start player1 player2 make-shadow lang)
  (current-language lang)
  (let* ((g (game (make-shadow) player1 player2 player1 '()))
         (frame (new kg-frame%
                     [label "To knot or not to knot"]
                     [width 600]
                     [height 450]))
 
         )
    (set-global-game! g)
;    (set-game-solver! global-game (lambda (k) (send canvas solve)))
    (game-start global-game)
    (send frame show #t)
    frame))

(define frame (start human-player-play human-player-play make-shadow-7-4 'fr))

;(start human-player-play knotter-play make-shadow-7-4 'fr)
;(start human-player-play unknotter-play make-shadow-7-4 'fr)