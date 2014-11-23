#lang racket

(require racket/gui)
(require srfi/29) ; l10n
(require "../syntaxes.rkt")
(require "../game.rkt")
(require "../knot.rkt")
(require "kg-canvas.rkt")

 
(provide kg-frame%)

(define new-game-choice-tree
  (let* ([fplayer1 '()]
         [fplayer2 '()]
         [player-computer '()])

    `(((Against_the_computer 
        ,(λ () '()) 
        ((Computer_starts 
          ,(λ () 
             (set! fplayer1 (λ () player-computer))
             (set! fplayer2 (λ () human-player-play))))
         (You_start
          ,(λ () 
             (set! fplayer1 (λ () human-player-play))
             (set! fplayer2 (λ () player-computer)))))
        ((You_knot
          ,(λ ()
             (set! player-computer unknotter-play)
             (set-global-game! (new-game make-shadow-7-4 
                                         #:player1 (fplayer1) 
                                         #:player2 (fplayer2)))))
         (You_do_NOT_knot
          ,(λ ()
             (set! player-computer knotter-play)
             (set-global-game! (new-game make-shadow-7-4  
                                         #:player1 (fplayer1) 
                                         #:player2 (fplayer2)))))))
       (2_players
        ,(λ () (set-global-game! (new-game make-shadow-7-4  
                                           #:player1 human-player-play 
                                           #:player2 human-player-play))))))))

;       (over_network
;        ,(λ () '())
;        ((send_invitation
;          ,(λ () '())
;          ((Your_guest_starts
;            ,(λ ()
;               (set-global-game! (new-game make-shadow-7-4  
;                                         #:player1 network-play 
;                                         #:player2 human-player-play))))
;           (You_start
;            ,(λ ()
;               (set-global-game! (new-game make-shadow-7-4  
;                                         #:player1 human-player-play
;                                         #:player2 network-play))))))
;         (wait_invitation
;            ,(λ ()
;               (let ([players (accept-invitation)])
;                 (set-global-game! (new-game make-shadow-7-4  
;                                         #:player1 (car players)
;                                         #:player2 (cdr players))))))))))))

(define al-players-user-choices
  `(((Against_the_computer Computer_starts You_knot) . (,unknotter-play . ,human-player-play))
    ((Against_the_computer Computer_starts You_do_NOT_knot) . (,knotter-play . ,human-player-play))
    ((Against_the_computer You_start You_knot) . (,human-player-play . ,unknotter-play))
    ((Against_the_computer You_start You_do_NOT_knot) . (,human-player-play . ,knotter-play))
    ((2_players) . (,human-player-play . ,human-player-play))))

(define (get-choices-from-user-l10n title message choices)
  (get-choices-from-user (localized-template 'knot title) 
                         (localized-template 'knot message)
                         (map (λ (m) (localized-template 'knot m))
                              choices)))

(define (get-choice-tree-from-user title message choice-trees)
  (when (not (null? choice-trees))
    (let* ([current-tree (car choice-trees)]
           [choices (map car current-tree)]
           [current-choice
            (get-choices-from-user-l10n title message choices)])
      ((cadr (list-ref current-tree (car current-choice))))
      (get-choice-tree-from-user 
       title message (cddr (list-ref current-tree (car current-choice))))
      (get-choice-tree-from-user 
       title message (cdr choice-trees)))))


(define new-game-choice-tree2
  '(((Against_the_computer 
      ((Computer_starts) (You_start))
      ((You_knot) (You_do_NOT_knot)))
     (2_players))))
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
    (super-new)
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
            (lambda (mi ce)
              (get-choice-tree-from-user 'New_game_etc "" new-game-choice-tree))]))

    
    (define item-new-game-etc2 
      (new menu-item%
           [label "New_game_etc 2"]
           [parent menu-game]
           [callback 
            (λ (mi ce)
              (let1 players
                    (cdr (get-players-from-user-choices
                     (get-choice-tree-from-user2 'New_game_etc "" new-game-choice-tree2)))
                    (set-global-game! 
                     (new-game make-shadow-7-4 
                               #:player1 (car players)
                               #:player2 (cdr players)))))]))
    
    (define status-bar
          (new message%
               [parent this]
               [label "toto"]))
    
    (send status-bar auto-resize  #true)

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
