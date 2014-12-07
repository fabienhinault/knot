#lang racket


(require "syntaxes.rkt")
(require "utils.rkt")
(require "knot.rkt")
(require "knot-node.rkt")

(provide (struct-out game))
(provide game-start)
(provide game-play)
(provide add-game-play-observer)
(provide change-game-current-player!)
(provide knotter-play)
(provide unknotter-play)
(provide dumb-computer-play)
(provide random-computer-play)
(provide network-play)
(provide set-network-ports!)
(provide accept-invitation)
(provide human-player-play)
(provide game-status)
(provide global-game)
(provide set-global-game!)
(provide add-set-global-game-observer)
(provide new-game)



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

(define-values (game-play add-game-play-observer)
  (let  ([observers '()])
    (values
     (lambda (g knode pnode)
       (set-knot-node-over! knode pnode)
       (if (knot-complete? (game-knot g))
          ((game-solver g) (game-knot g))
          (begin
            (change-game-current-player! g)
            ((game-current-player g) g)))
      (map (lambda (f) (f)) observers))
     (lambda (f) (set! observers (cons f observers))))))

(define (game-play-z g z)
  '())

(define (change-game-current-player! g)
  (set-game-current-player! 
   g 
   (if (equal? (game-player1 g) (game-current-player g))
       (game-player2 g)
       (game-player1 g))))

(define-values
  (network-play set-network-ports!)
  (let* ([in '()]
         [out '()])
        (values
         (λ (g) (game-play-z g (string->number (read in))))
         (λ (new-in new-out) 
           (set! in new-in)
           (set! out new-out)))))

(define (accept-invitation)
  '(()()))

(define (x-knotter-play g predicate?)
  (let* ((k (game-knot g))
         (sols (predicate? k)))
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

(define (network-player k) '())

(define (game-status g)
  (let1 game-status-messages
        (list (list human-player-play human-player-play '2_players)
              (list knotter-play human-player-play 'Against_the_computer 'You_do_NOT_knot)
              (list human-player-play knotter-play 'Against_the_computer 'You_do_NOT_knot 'You_start)
              (list unknotter-play human-player-play 'Against_the_computer 'You_knot)
              (list human-player-play unknotter-play 'Against_the_computer 'You_knot 'You_start))
        (cddr (findf (λ (l) (and (equal? (car l) (game-player1 g))
                                 (equal? (cadr l) (game-player2 g))))
                     game-status-messages))))

(define global-game (game (make-shadow-7-4) human-player-play human-player-play human-player-play '()))

(define set-global-game-observers '())
(define (add-set-global-game-observer f)
  (set! set-global-game-observers
        (cons f set-global-game-observers)))
(define (set-global-game! g)
  (set! global-game g)
  (map (λ (f) (f)) set-global-game-observers))


(define (new-game make-shadow
                  #:old-game [old-game '()] 
                  #:player1 [player1 (game-player1 old-game)] 
                  #:player2 [player2 (game-player2 old-game)])
  (let1 g (game (make-shadow) 
                player1 
                player2 
                player1 
                '())
        (game-start g)
        g
        ))