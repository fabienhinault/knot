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

(define (generate-random-vertices n xmax ymax)
  (map (λ(_) (make-rectangular (* (random) xmax) (* (random) ymax)))
                  (range n)))
    
(define (generate-random-shadow n xmax ymax dt)
  (let* ([trace (knot-shadow-shuffle n)]
         [zs (generate-random-vertices n xmax ymax)]
         [edges (trace-edges trace n)])
    (displayln trace)
    (displayln zs)
    (displayln edges)
    (do () (#f)
      (set! zs (update-positions dt zs edges n))
      (displayln zs)
      (read-line)
      )))
         

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

; repulsive force created by "by" on "on". 
; The force has same direction as vector "by on"
;
;       by                   on
;        .                    .----->
;                                force (same dir. as (on - by))
;
(define (push-force on by)
  (let* ([charge 100000]
         [d (- on by)]
         [l (magnitude d)])
    (if (> l 0)
        (make-polar (/ charge (* l l)) (angle d))
        0)))
(let* ([z1 1+i]
       [z2 -1-i])
  (check-equal?
   (angle (push-force z1 z2)) 
   (angle (- z1 z2))))
  
; attractive force created by "by" on "on".
; The force has same direction as vector "on by"
;
;       by                   on
;        .                <---.
;                           force (same dir. as (by - on))
;
(define (pull-force on by)
  (let* ([stiffness 0.5])
    (* stiffness (- by on))))

(let* ([z1 1+i]
       [z2 -1-i])
  (check-equal?
   (angle (pull-force z1 z2)) 
   (angle (- z2 z1)))
  (check-equal?
   (angle (pull-force z1 z2)) 
   (- (angle (push-force z1 z2)) pi)))
  
  

(define (push-result zi zs)
  (foldl + 0 (map (λ (z) (push-force zi z))
                  zs)))

(define (pull-result i zs edges)
  (let* ([zi (list-ref zs i)])
    (foldl + 0 (map (λ (z) (pull-force zi z))
                    (map (λ (j) (list-ref zs j)) (assoc i edges))))))

(define (velocity z1 z2 f dt)
  (* dt (f z1 z2)))

(define (update-position dt i zs edges)
  (let* ([zi (list-ref zs i)]
         [push (foldl + 0 (map (λ (z) (push-force zi z))
                               zs))]
         [pull (foldl + 0 (map (λ (z) (pull-force zi z))
                               (map (λ (j) (list-ref zs j)) (assoc i edges))))])
    (+ zi (* dt (+ push pull)))))

(define (update-positions dt zs edges n)
  (map 
   (λ (i) (update-position dt i zs edges))
   (range n)))
         