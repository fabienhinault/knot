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
(provide human-player-play)




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