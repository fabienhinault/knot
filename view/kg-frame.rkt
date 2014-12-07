#lang racket

(require racket/gui)
(require srfi/29) ; l10n
(require "locale.rkt")
(require "../syntaxes.rkt")
(require "../game.rkt")
(require "../knot.rkt")
(require "kg-canvas.rkt")


(provide kg-frame%)

(define (wait-for-invitation)
  (let1 listener (tcp-listen (string->number (get-text-from-user "Nouvelle partie" "port d'écoute")))
        ;(send frame set-status! "en attente d'une invitation")
        (let-values ([(in out) (tcp-accept listener)])
          (set-network-ports! in out)))
  network-play)

(define (send-invitation)
  (get-text-from-user "Nouvelle partie" "IP:port"))

(define al-players-user-choices
  `(((Against_the_computer Computer_starts You_knot) . ,(λ ()(cons unknotter-play human-player-play)))
    ((Against_the_computer Computer_starts You_do_NOT_knot) . ,(λ ()(cons knotter-play human-player-play)))
    ((Against_the_computer You_start You_knot) . ,(λ ()(cons human-player-play unknotter-play)))
    ((Against_the_computer You_start You_do_NOT_knot) . ,(λ ()(cons human-player-play knotter-play)))
    ((2_players) ,(λ ()(cons human-player-play human-player-play)))
    ((Over_the_network Send_invitation) . ,send-invitation)
    ((Over_the_network Wait_for_invitation You_start) . ,(λ () (cons human-player-play (wait-for-invitation))))
    ((Over_the_network Wait_for_invitation Other_player_starts) . ,(λ () (cons (wait-for-invitation) human-player-play)))))
  
  
  (define (get-choices-from-user-l10n title message choices)
    (get-choices-from-user (localized-template 'knot title) 
                           (localized-template 'knot message)
                           (map (λ (m) (localized-template 'knot m))
                                choices)))
  
  (define new-game-choice-tree2
    '(((Against_the_computer 
        ((Computer_starts) (You_start))
        ((You_knot) (You_do_NOT_knot)))
       (2_players)
       (Over_the_network
        ((Send_invitation) 
         (Wait_for_invitation
          ((You_start) (Other_player_starts))))))))
  
  
  
  ; choices-trees = new-game-choice-tree2
  ; current-tree = '((Against_the_computer ...
  ; choices = '(Against_the_computer 2_players)
  
  ; current-choice = 1
  ; (list-ref current-tree current-choice) = '(2_players ())
  
  ; current-choice = 0
  ; (list-ref current-tree current-choice) = '(Against_the_computer 
  ;                                            (((Computer_starts) 
  ;                                              (You_start))
  ;                                             ((You_knot) (You_do_NOT_knot)))
  ; choices-trees = '(((Computer_starts) (You_start))
  ;                   ((You_knot) (You_do_NOT_knot)))
  ; current-tree = '((Computer_starts) (You_start))
  
  (define (get-choice-tree-from-user2 title message choice-trees)
    (if (null? choice-trees) 
        '()
        (let* ([current-tree (car choice-trees)]
               [choices (map car current-tree)]
               [current-choice
                (car (get-choices-from-user-l10n title message choices))])
          (cons
           (list-ref choices current-choice)
           (append
            (get-choice-tree-from-user2
             title message (cdr (list-ref current-tree current-choice)))
            (get-choice-tree-from-user2
             title message (cdr choice-trees)))))))
  
  (define (get-players-from-user-choices user-choices)
    (assoc user-choices al-players-user-choices))
  
  (define kg-frame%
    (class frame%
      (init lang)
      (super-new)
      (current-language lang)
      (define menu-bar (new menu-bar% [parent this]))
      (define canvas (new kg-canvas%
                          [parent this]
                          [agame global-game]))
      (define menu-game (new menu%
                             (label (localized-template 'knot 'Game))
                             (parent menu-bar)))
      (define item-new-game 
        (new menu-item%
             [label (localized-template 'knot 'New_game)]
             [parent menu-game]
             [callback 
              (λ (mi ce)
                (set-global-game! (new-game make-shadow-7-4 #:old-game global-game)))]))
      
      (define item-new-game-etc 
        (new menu-item%
             [label (localized-template 'knot 'New_game_etc)]
             [parent menu-game]
             [callback 
              (λ (mi ce)
                (let1 players
                      ((cdr (get-players-from-user-choices
                             (get-choice-tree-from-user2 'New_game_etc "" new-game-choice-tree2))))
                      (set-global-game! 
                       (new-game make-shadow-7-4 
                                 #:player1 (car players)
                                 #:player2 (cdr players)))))]))
      
      (define status-bar
        (new message%
             [parent this]
             [label "toto"]))
      
      (send status-bar auto-resize  #true)
      
      (define/public (set-status! msg)
        (send status-bar set-label msg)
        (send status-bar refresh))
      
      (define/public (update-status)
        (let1 game-status-label 
              (map (λ (sym) (localized-template 'knot sym)) 
                   (game-status global-game))
              (send status-bar set-label
                    (foldl (λ (str1 str2) (string-append str1 " | " str2))
                           (car game-status-label)
                           (cdr game-status-label)))
              (send status-bar refresh)))
      
      (add-set-global-game-observer (λ () (send this update-status)))
      
      ))
  