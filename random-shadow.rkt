#lang racket
(require "syntaxes.rkt")
(require racket/match)

(require rackunit)

(define (generate-random-shadow nb-nodes)
  (let1 half (range nb-nodes)
  (shuffle (append half half))))

(define-struct (knot-shadow-insert-exception exn:fail:user)())

(define (knot-shadow-insert e l [f? knot-shadow-insert?]  [=? equal?])
  (let1 p (f? l)
        (cond [(null? l) (list e)]
              ; if e == (car l), jump 3 numbers if possible, to avoid same number too near
              [(and (=? (car l) e) (not (null? (cdr l))) (not (null? (cddr l)))) 
               (list* (car l) (cadr l) (caddr l)(knot-shadow-insert e (cdddr l) f? =?))]
              ; 1 elt list == e -> failure
              [(and (=? (car l) e) (null? (cdr l))) (raise (make-knot-shadow-insert-exception "" (current-continuation-marks)))]
              ; 1 elt list != e -> (e x) or (x e)
              [(and (null? (cdr l)) p) (cons e l)]
              [(and (null? (cdr l)) (not p)) (list (car l) e)]
              ; 2 elt list == (e x) -> failure
              [(and (=? (car l) e) (null? (cddr l))) (raise (make-knot-shadow-insert-exception "" (current-continuation-marks)))]
              ; second element == e -> send to the case "e == (car l)"
              [(=? (cadr l) e) (cons (car l) (knot-shadow-insert e (cdr l) f? =?))]
              [p (cons e l)]
              [else (cons (car l) (knot-shadow-insert e (cdr l) f? =?))])))

(define (knot-shadow-insert? l)
  (< (random) (/ 1.0 (add1 (length l)))))

(define (knot-shadow-shuffle n [f? knot-shadow-insert?]  [=? equal?])
  (foldl (λ (e l)
           (with-handlers 
               ([knot-shadow-insert-exception? 
                 (λ (e) (knot-shadow-insert e l (λ (_) #t) =?))])
             (knot-shadow-insert e l f? =?)))
         (range n)
         (range n)))


(check-equal?
 (knot-shadow-insert 0 '() knot-shadow-insert? equal?)
 '(0))
(check-equal?
 (knot-shadow-insert 0 '() (λ (l) #t) equal?)
 '(0))
(check-not-false
 (member (knot-shadow-insert 0 '(1) knot-shadow-insert? equal?)
 '((0 1) (1 0))))
(check-equal?
 (knot-shadow-insert 0 '(1) (λ (l) #t) equal?)
 '(0 1))
;(check-false
; (knot-shadow-insert 0 '(0) knot-shadow-insert? equal?))
;(check-false
; (knot-shadow-insert 0 '(0 1) (λ (l) #t) equal?))
(check-equal?
 (knot-shadow-insert 0 '(0 1 2) (λ (l) #t) equal?)
 '(0 1 2 0))
(check-equal?
 (knot-shadow-insert 0 '(0 1 2)  knot-shadow-insert? equal?)
 '(0 1 2 0))
(check-equal? 
 (knot-shadow-insert 0 '(1) (λ (l) #t) equal?)
 '(0 1))

(check-equal? 
 (with-handlers 
      ([knot-shadow-insert-exception? 
        (λ (e) '(0 1))])
    (knot-shadow-insert 0 '(0) (λ (l) #t) equal?)
     )
 '(0 1))

(check-equal? 
 (with-handlers 
      ([knot-shadow-insert-exception? 
        (λ (e) (knot-shadow-insert 0 '(1) (λ (l) #t) equal?))])
    (knot-shadow-insert 0 '(0) (λ (l) #t) equal?)
     )
 '(0 1))
(check-equal?
 (foldl (λ (e l) (knot-shadow-insert e l (λ (l) #t)))
        (range 3)
        (range 3))
 '(2 0 1 2 0 1))
