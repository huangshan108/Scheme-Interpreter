(define (repeat k fn)
  ; Repeat fn k times.
  (if (> k 1)
      (begin (fn) (repeat (- k 1) fn))
      (fn)))


(define (set_data scaler)
  (define outer_radius (* 100 scaler))
  (define (inner_radius outer_radius)
    (- outer_radius fake_radius))

  (define radius (/ outer_radius 8))
  (define fake_radius (/ radius (sine 45)))
  (define angle (/ 360 60))

  (define double-semi 
    (lambda()
    (set-color "#FC434F" "black")
    (set-color "#63AFF5" "white")
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
  (if (< ratio 0.09) 
    0
     (begin (set_data ratio)
           (rt 135)
           (fd (* ratio default_ratio (* 12.5 (sqrt 2))))
           (rt 90)
           (pu)
           (circle (* 82.322 ratio default_ratio) 6)
           (pd)
           (rt 135)
           (draw_one (* ratio default_ratio)))
  )
)


(define columns 4)
(define rows 3)
(define (draw_bottom len num)
  (cond ((> len 0) (begin (pu) (goto (- (* 200 (- columns len)) 300) (- 300 (* 200 (- rows num)))) (pd) 
          (seth 45) (draw_one 1) 
          (draw_bottom (- len 1) num)))
        ((> num 1) (draw_bottom columns (- num 1)))
        (else 0)))

(define columns2 3)
(define rows2 2)
(define (draw_top len num)
  (cond ((> len 0) (begin (pu) (goto (- (* 200 (- columns2 len)) 200) (- 200 (* 200 (- rows2 num)))) (pd) 
          (seth 45) (draw_one 1) 
          (draw_top (- len 1) num)))
        ((> num 1) (draw_top columns2 (- num 1)))
        (else 0)))

(draw_bottom columns rows)
(draw_top columns2 rows2)
(exitonclick)







