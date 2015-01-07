(define (repeat k fn)
  ; Repeat fn k times.
  (if (> k 1)
      (begin (fn) (repeat (- k 1) fn))
      (fn)))


(define (set_data scaler)
  (define outer_radius (* 120 scaler))
  (define (inner_radius outer_radius)
    (- outer_radius fake_radius))

  (define radius (* 15 scaler))
  (define fake_radius (/ radius (sine 45)))
  (define angle (/ 360 60))

  (define double-semi 
    (lambda()
    (set-color "blue" "black")
    (set-color "yellow" "white")
    )
  )


  (define (set-color color1 color2)
  (pd)
  (begin_fill)
  (color color1)
  (circle radius 90)
  (lt 90)
  (circle radius 90)
  (end_fill)

  (pu)
  (begin_fill)
  (color color2)
  (rt 135)
  (circle (inner_radius outer_radius) angle)
  (rt 135)
  (circle radius 90)
  (rt 135)
  (circle (- outer_radius) angle)
  (rt 135)
  (circle radius 90)
  (end_fill)
  (rt 135)
  (circle (inner_radius outer_radius) angle)
  (rt 135)
  )

  (repeat 30 double-semi)

)

(speed 0)

(define default_ratio 0.82322)

  (define (draw_one ratio)
    (if (< ratio 0.05) 
      0
      (begin (set_data ratio)
             (rt 135)
             (fd (* ratio default_ratio (* 15 (sqrt 2))))
             (rt 90)
             (circle (* 98.768 ratio default_ratio) 6)
             (rt 135)
             (draw_one (* ratio default_ratio)))
    )
  )
  (draw_one 1)



(draw_one)






(define unk (lambda ()
(color "red")

(begin_fill)
(fd 100)
(rt 120)
(fd 100)
(rt 120)
(fd 100)

(end_fill)
))

(define (run)
(color "black")
(rt 90)
(pu)
(fd 125)
(pd)
(lt 90)
(begin_fill)
(circle 125)
(lt 90)
(pu)
(fd inner)
(pd)
(rt 90)
(circle 105)
(end_fill)
(pu)
(lt 90)
(fd 105)
(pd)
(repeat 3 unk)
)

;(run)

(define (repeat k fn)
  ; Repeat fn k times.
  (if (> k 1)
      (begin (fn) (repeat (- k 1) fn))
      (fn)))

(define (tri fn)
  ; Repeat fn 3 times, each followed by a 120 degree turn.
  (repeat 3 (lambda () (fn) (lt 120))))

(define (sier d k)
  ; Draw three legs of Sierpinski's triangle to depth d.
  (tri (lambda ()
         (if (= k 1) (fd d) (leg d k)))))

(define (leg d k)
  ; Draw one leg of Sierpinski's triangle to depth d.
  (sier (/ d 2) (- k 1))
  (penup)
  (fd d)
  (pendown))

;(sier 400 6)