#lang racket

(require racket/gui)
(require racket/draw)
(require rackunit)
;(require srfi/1) ; list-index
(require srfi/26) ; cut cute
(require "syntaxes.rkt")
(require "utils.rkt")
(require "path-node.rkt")
(require "knot-node.rkt")
(require "knot.rkt")
(require "game.rkt")
(require "view/kg-canvas.rkt")
(require "view/kg-frame.rkt")


(define (start player1 player2 make-shadow lang)
;  (current-language lang)
  (let* ((g (game (make-shadow) player1 player2 player1 '()))
         (frame (new kg-frame%
                     [label "To knot or not to knot"]
                     [width 600]
                     [height 450]
                     [lang lang])) 
         )
    (set-global-game! g)
    (game-start global-game)
    (send frame show #t)
    frame))

(define frame (start human-player-play human-player-play make-shadow-7-4 'fr))

;(start human-player-play knotter-play make-shadow-7-4 'fr)
;(start human-player-play unknotter-play make-shadow-7-4 'fr)