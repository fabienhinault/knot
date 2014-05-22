#lang racket
(require "syntaxes.rkt")
(require "utils.rkt")
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
    
(define (generate-random-shadow n xmax ymax)
  (let* ([trace (knot-shadow-shuffle n)]
         [zs (generate-random-vertices n xmax ymax)]
         [edges (trace-edges trace n)])
    (displayln trace)
    (displayln zs)
    (displayln edges)
    (do () (#f)
      (set! zs (update-positions zs edges n))
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

;(define (push-force on by)
;  (let* ([charge 100000]
;         [d (- on by)]
;         [l (magnitude d)])
;    (if (> l 0)
;        (make-polar (/ charge (* l l)) (angle d))
;        0)))
;
;(define (sqr-magnitude z)
;  (let* ([r (real-part z)]
;         [i (imag-part z)])
;    (+ (* r r) (* i i))))

;(define (push-force on by)
;  (let* ([d (- on by)]
;         [l (magnitude d)])
;    (cond  
;      [(>= l 200) (make-polar 2 (angle d))]
;      [(> l 0) (make-polar (- 200 l) (angle d))]
;      [else 0])))

(define ref-distance 100)
(define low-value 2)

; push must compensate pull around ref-distance
(define (push on by amortizement)
  (let* ([d (- on by)]
         [l (magnitude d)])
    (cond  
      [(>= l (* 2 ref-distance)) (make-polar (/ low-value l) (angle d))]
      [(> l 0) (make-polar (* amortizement (- (* 2 ref-distance) l)) (angle d))]
      [else 0])))

(let* ([z1 100]
       [z2 -100])
  (check-equal?
   (angle (push z1 z2 .2)) 
   (angle (- z1 z2))))
  
; attractive force created by "by" on "on".
; The force has same direction as vector "on by"
;
;       by                   on
;        .              <-----.
;                        force (same dir. as (by - on))
;

;(define (pull-force on by)
;  (- by on))

(define (pull on by amortizement)
  (* amortizement (- by on)))

(let* ([z1 1+i]
       [z2 -1-i])
  (check-equal?
   (angle (pull z1 z2 .2)) 
   (angle (- z2 z1)))
  (check-equal?
   (angle (pull z1 z2 .2)) 
   (- (angle (push z1 z2 .2)) pi)))

(define (push-result zi zs)
  (foldl + 0 (map (λ (z) (push zi z))
                  zs)))

(define (pull-result i zs edges)
  (let* ([zi (list-ref zs i)])
    (foldl + 0 (map (λ (z) (pull zi z))
                    (map (λ (j) (list-ref zs j)) (assoc i edges))))))

(define (velocity z1 z2 f dt)
  (* dt (f z1 z2)))

(define (update-position i zs edges amortizement)
  (let* ([zi (list-ref zs i)]
         [push-res (foldl + 0 (map (λ (z) (push zi z amortizement))
                               zs))]
         [pull-res (foldl + 0 (map (λ (z) (pull zi z amortizement))
                               (map (λ (j) (list-ref zs j)) (assoc i edges))))])
    (+ zi push-res pull-res)))

(define (update-positions zs edges n amortizement)
  (map 
   (λ (i) (update-position i zs edges amortizement))
   (range n)))

;(require racket/gui) 
;(let* ([n 4]
;         [zs (generate-random-vertices n 600 600)]
;         [edges '((0 1 2) (1 0 2) (2 0 1 3) (3 2))]
;         [frame (new frame%
;                     [label ""]
;                     [width 600]
;                     [height 600])]
;         [canvas (new canvas%
;                      [parent frame]
;                      [paint-callback
;                       (λ (this dc)
;                         (send dc set-pen "black" 5 'solid)
;                         (for ([z zs])
;                           (send dc draw-point (real-part z) (imag-part z))))])])
;    (for ([i (range 50)])
;      (set! zs (update-positions zs edges n 0.1)))
;    (send frame show #t))

;(let* ([n 4]
;         [zs (generate-random-vertices n 600 600)]
;         [edges '((0 1 2) (1 0 2) (2 0 1 3) (3 2))])
;    (displayln zs)
;    (displayln edges)
;    (do () (#f)
;      (set! zs (update-positions zs edges n))
;      (displayln zs)
;      (read-line)))


;(let* ([n 4]
;         [zs (generate-random-vertices n 600 600)]
;         [edges '((0 1 2) (1 0 2) (2 0 1 3) (3 2))]
;         [frame (new frame%
;                     [label ""]
;                     [width 600]
;                     [height 600])]
;         [canvas (new canvas%
;                      [parent frame]
;                      [paint-callback
;                       (λ (this dc)
;                         (send dc set-pen "black" 5 'solid)
;                         (for ([z zs])
;                           (send dc draw-point (real-part z) (imag-part z)))
;                         (send dc set-pen "black" 1 'solid)
;                         (for ([edge edges])
;                           (let ([start (list-ref zs (car edge))])
;                             (for ([end (cdr edge)])
;                               (send dc draw-line
;                                     (real-part start)
;                                     (imag-part start)
;                                     (real-part (list-ref zs end))
;                                     (imag-part (list-ref zs end))))))
;                         )])])
;    (for ([i (range 50)])
;      (set! zs (update-positions zs edges n 0.1)))
;    (send frame show #t))