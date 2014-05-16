#lang racket
(require "syntaxes.rkt")
(require "utils.rkt")
(require racket/match)
(require racket/promise)
(require rackunit)



(define-struct (knot-shadow-insert-exception exn:fail:user)())

(define (knot-shadow-insert e l [f? knot-shadow-insert?] [=? equal?])
  (let1 p (delay (f? l))
        (cond [(null? l) (list e)]
              ; if e == (car l), jump 3 numbers if possible, to avoid same number too near
              [(and (=? (car l) e) (not (null? (cdr l))) (not (null? (cddr l)))) 
               (list* (car l) (cadr l) (caddr l)(knot-shadow-insert e (cdddr l) f? =?))]
              ; 1 elt list == e -> failure
              [(and (=? (car l) e) (null? (cdr l))) 
               (raise (make-knot-shadow-insert-exception "" (current-continuation-marks)))]
              ; 1 elt list != e -> (e x) or (x e)
              [(and (null? (cdr l)) (force p)) (cons e l)]
              [(and (null? (cdr l)) (not (force p))) (list (car l) e)]
              ; 2 elt list == (e x) -> failure
              [(and (=? (car l) e) (null? (cddr l))) 
               (raise (make-knot-shadow-insert-exception "" (current-continuation-marks)))]
              ; second element == e -> send to the case "e == (car l)"
              [(=? (cadr l) e) 
               (cons (car l) (knot-shadow-insert e (cdr l) f? =?))]
              [(force p) (cons e l)]
              [else (cons (car l) (knot-shadow-insert e (cdr l) f? =?))])))

(define (knot-shadow-insert? l)
  (< (random) (/ 1.0 (add1 (length l)))))

(define test-knot-shadow-insert-modulo?
  (λ (thresh)
    (let* ([counter 0]
           [threshold thresh])
      (λ ( l)
        (set! counter (add1 counter))
        (if (equal? counter threshold)
            (begin
              (set! counter 0)
              #t)
            #f)))))

(define test-knot-shadow-insert-list?
  (λ (lb)
    (let* ([current lb]
           [fix lb])
      (λ ( l)
        (set! current (cdr current))
        (when (equal? current '())
          (set! current fix))
        (car current)))))

(define (knot-shadow-shuffle n [f? knot-shadow-insert?]  [=? equal?])
  (foldl (λ (e l)
           (with-handlers 
               ([knot-shadow-insert-exception? 
                 (λ (x) (knot-shadow-insert e l (λ (_) #t) =?))])
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

(define (shadow-trace? l)
  (let* ([n (length l)]
         [1st (first l)]
         [2nd (second l)]
         [nth (last l)]
         [n-1th (last (take l (sub1 n)))])
    
    (not (or (equal? 1st nth)
             (equal? 1st n-1th)
             (equal? 2nd nth)))))

(define (generate-random-shadow n xmax ymax)
  (let* ([trace (knot-shadow-shuffle n)]
         [zs (map (λ(_) (make-rectangular (* (random) xmax) (* (random) ymax)))
                  (range n))])
    '()))
    
(define (trace-edges trace n)
  (let* ([neighbours (map list (range n))]
         [edges (append (map list trace (left-cycle-1 trace))
                        (map list trace (right-cycle-1 trace)))])
    (map (λ (i) (cons i 
                      (remove-duplicates
                       (sort (map cadr 
                                  (filter (λ (_) (equal? (car _) i)) 
                                          edges))
                             <))))
         (range n))))

(define (pushForce z1 z2)
  (let* ([charge 100000]
         [d (- z1 z2)]
         [l (magnitude d)])
    (if (> l 0)
        (make-polar (/ charge l) (angle d))
        0)))
  
(define (pullForce z1 z2)
  (let* ([stiffness 0.5])
    (* stiffness (- z2 z1))))

(define (velocity z1 z2 f dt)
  (* dt (f z1 z2)))

(define (update-position dt i zs)
  (let* ([vertex-pos (list-ref zs i)]
         [push '()])
    '()))
         