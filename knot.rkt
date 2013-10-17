#lang racket
(require racket/gui)
(require racket/draw)
(require racket/match)

(define (int r) (inexact->exact (round r)))

(define (velocity sintheta costheta sinphi cosphi tension)
  (min 4.0
       (/
        (+ 2.0
           (* (sqrt 2) (- sintheta (/ sinphi 16)) (- sinphi (/ sintheta 16)) (- costheta cosphi)))
        (* 3.0 (+ 1 (* 0.5 (- (sqrt 5) 1) costheta)
                  (* 0.5 (- 3 (sqrt 5)) cosphi)))
        tension)))

(define (left-velocity sintheta costheta sinphi cosphi)
  (velocity sinphi cosphi sintheta costheta))

(define d 150)
(define d*sqrt3/6 (inexact->exact (round (/ (* (sqrt 3) d) 6))))
(define d/3sqrt2 (int(/ d (* 3 (sqrt 2)))))

(define xs (list 375 525 450))
(define ys (list 475 475 (int (+ 475 (/ (* (sqrt 3) d) 2)))))
(define psi (/ (* 2 pi) 3))
;(define theta0 (- (/ pi 12)))
(define theta0 (- (* pi 0.15)))
(define theta1 (- (- (/ (* 2 pi) 3)) theta0 ))
(define thetas (list theta0 theta1))

(define phis
  (map
   (lambda (t) (- 0 psi t))
   thetas))

(match-define (list phi0 phi1) phis)


(define dxs (list 
             (int(*(/ d 3) (cos (/ pi 12)))) 
             (- (int(*(/ d 3) (sin (/ pi 12))))) 
             (- d/3sqrt2)))


(define dys (list (- (int(*(/ d 3) (sin (/ pi 12))))) (int(*(/ d 3) (cos (/ pi 12)))) (- d/3sqrt2)))

(define tensions 
  (list 1
        (/ (+ (* 2 theta0) (- (* 4 phi1)) (* 6 theta1)) 
           (* 2 (+ phi0 theta1)))))
        
;(define (fdx0 point-index)
;  (let* ((next-point-index (modulo (+ 1 point-index) (length xs)))
;         (delta_x (- (list-ref xs next-point-index) (list-ref xs point-index)))
;         (delta_y (- (list-ref ys next-point-index) (list-ref ys point-index))))
;    (* (- (* delta_x (cos theta0)) (* delta_y (sin phi0)))
;       (velocity (sin theta0) (cos theta0) (sin phi0) (cos phi0)))))
;
;
;(define (fdx1 point-index)
;  (let* ((next-point-index (modulo (+ 1 point-index) (length xs)))
;         (delta_x (- (list-ref xs next-point-index) (list-ref xs point-index)))
;         (delta_y (- (list-ref ys next-point-index) (list-ref ys point-index))))
;    (* (- (* delta_x (cos theta1)) (* delta_y (sin phi1)))
;       (velocity (sin theta1) (cos theta1) (sin phi1) (cos phi1)))))


(define (fdx+ move-index point-index)
  (let* ((next-point-index (modulo (+ 1 point-index) (length xs)))
         (delta_x (- (list-ref xs next-point-index) (list-ref xs point-index)))
         (delta_y (- (list-ref ys next-point-index) (list-ref ys point-index)))
         (theta (list-ref thetas move-index))
         (phi (list-ref phis (- 1 move-index)))
         (tension (list-ref tensions move-index)))
    (* (- (* delta_x (cos theta)) (* delta_y (sin theta)))
       (velocity (sin theta) (cos theta) (sin phi) (cos phi) tension))))

(define (fdy+ move-index point-index)
  (let* ((next-point-index (modulo (+ 1 point-index) (length xs)))
         (delta_x (- (list-ref xs next-point-index) (list-ref xs point-index)))
         (delta_y (- (list-ref ys next-point-index) (list-ref ys point-index)))
         (theta (list-ref thetas move-index))
         (phi (list-ref phis (- 1 move-index)))
         (tension (list-ref tensions move-index)))
    (* (+ (* delta_y (cos theta)) (* delta_x (sin theta)))
       (velocity (sin theta) (cos theta) (sin phi) (cos phi) tension))))

(define (fdx- move-index point-index)
  (let* ((prev-point-index (modulo (- point-index 1) (length xs)))
         (delta_x (- (list-ref xs point-index) (list-ref xs prev-point-index)))
         (delta_y (- (list-ref ys point-index) (list-ref ys prev-point-index)))
         (theta (list-ref thetas (- 1 move-index)))
         (phi (list-ref phis move-index))
         (tension (list-ref tensions (- 1 move-index))))
    (* (- (- (* delta_y (sin phi))) (* delta_x (cos phi)))
       (velocity (sin phi) (cos phi) (sin theta) (cos theta) tension))))

(define (fdy- move-index point-index)
  (let* ((prev-point-index (modulo (- point-index 1) (length xs)))
         (delta_x (- (list-ref xs point-index) (list-ref xs prev-point-index)))
         (delta_y (- (list-ref ys point-index) (list-ref ys prev-point-index)))
         (theta (list-ref thetas (- 1 move-index)))
         (phi (list-ref phis move-index))
         (tension (list-ref tensions (- 1 move-index))))
    (* (- (+ (* delta_y (cos phi)) (- (* delta_x (sin phi)))))
       (velocity (sin phi) (cos phi) (sin theta) (cos theta) tension))))

(define (fd<xy><+->ss f) 
  (map 
   (lambda (move-index) 
     (map 
      (lambda (point-index)
        (f move-index point-index))
      '(0 1 2)))
   '(0 1)))

(match-define (list dx+ss dx-ss dy+ss dy-ss)
  (map
   fd<xy><+->ss
   (list fdx+ fdx- fdy+ fdy-)))


(define (x_0- x dx dy) (- x dx))
(define (y_0- y dx dy) (- y dy))
(define (x_0+ x dx dy) (+ x dx))
(define (y_0+ y dx dy) (+ y dy))

(define (x_1- x dx dy) (- x dy))
(define (y_1- y dx dy) (+ y dx))
(define (x_1+ x dx dy) (+ x dy))
(define (y_1+ y dx dy) (- y dx))

(define x+s (list x_0+ x_1+))
(define y+s (list y_0+ y_1+))
(define x-s (list x_0- x_1-))
(define y-s (list y_0- y_1-))

(define (xy+- fl coords)
  (lambda (move-index point-index)
    ((list-ref fl move-index)
     (list-ref coords point-index)
     (list-ref dxs point-index)
     (list-ref dys point-index))))

;(define x+ (xy+- x+s xs))
;(define x- (xy+- x-s xs))
;(define y+ (xy+- y+s ys))
;(define y- (xy+- y-s ys))


(define (x+ move-index point-index)
  (+ (list-ref xs point-index)
     (fdx+ move-index point-index)))
(define (y+ move-index point-index)
  (+ (list-ref ys point-index)
     (fdy+ move-index point-index)))
(define (x- move-index point-index)
  (+ (list-ref xs point-index)
     (fdx- move-index point-index)))
(define (y- move-index point-index)
  (+ (list-ref ys point-index)
     (fdy- move-index point-index)))

;(define knot%
;  (class dc-path%
;    (field (last-point-index 0))
;    (field (last-move-index 0))
;    (define (curve-to-n point-index move-index)
;      (send this
;            curve-to 
;            (x+ last-move-index last-point-index)
;            (y+ last-move-index last-point-index)
;            (x- move-index point-index)
;            (y- move-index point-index)
;            (list-ref xs point-index)
;            (list-ref ys point-index))
;      (set! last-point-index point-index)
;      (set! last-move-index move-index))
;    (public curve-to-n)
;    (super-new)))

(define knot%
  (class dc-path%
    (field (last-point-index 0))
    (field (last-move-index 0))
    (define (curve-to-n point-index move-index)
      (send this
            curve-to 
            (x+ last-move-index last-point-index)
            (y+ last-move-index last-point-index)
            (x- move-index point-index)
            (y- move-index point-index)
            (list-ref xs point-index)
            (list-ref ys point-index))
      (set! last-point-index point-index)
      (set! last-move-index move-index))
    (public curve-to-n)
    (super-new)))


(define knot (new knot%))
(send knot move-to (list-ref xs 0) (list-ref ys 0))
(send knot curve-to-n 1 1)
(send knot curve-to-n 2 0)
(send knot curve-to-n 0 1)
(send knot curve-to-n 1 0)
(send knot curve-to-n 2 1)
(send knot curve-to-n 0 0)

(define frame (new frame%
                   [label "Example"]
                   [width 300]
                   [height 300]))
(define canvas
  (new canvas% [parent frame]
       [paint-callback
        (lambda (canvas dc)
                (send dc draw-path knot))]))
;          (send dc set-pen (new pen% [color "black"]))
;          (send dc set-brush (new brush% [color "black"]))
;          (for ([x xs]
;                [y ys])
;            (send dc draw-ellipse x y 3 3))
;          
;          (send dc set-pen (new pen% [color "blue"]))
;          (send dc set-brush (new brush% [color "blue"]))
;          
;          (send dc draw-ellipse 
;                (+(car xs)(list-ref (list-ref dx+ss 0) 0))
;                (+(car ys)(list-ref (list-ref dy+ss 0) 0))
;                3 3)
;          (send dc draw-ellipse 
;                (+(car xs)(list-ref (list-ref dx+ss 1) 0))
;                (+(car ys)(list-ref (list-ref dy+ss 1) 0))
;                3 3)
;          )]))