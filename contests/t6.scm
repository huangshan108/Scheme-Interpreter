(define (repeat k fn)
  ; Repeat fn k times.
  (if (> k 1)
      (begin (fn) (repeat (- k 1) fn))
      (fn)))

(define outer_radius_num 120)
(define inner_radius_num 20)

(define (set_data scaler)
  (define outer_radius (* outer_radius_num scaler))
  (define (inner_radius outer_radius)
    (- outer_radius fake_radius))

  (define radius (/ outer_radius 6))
  (define fake_radius (/ radius (sine 45)))
  (define angle (/ 360 40))

  (define double-semi 
    (lambda()
    (set-color "#237DFB" "black")
    (set-color "#E4E336" "white")
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

  (repeat 20 double-semi)

)

(speed 0)

(define default_ratio (/ (- outer_radius_num (* inner_radius_num (sqrt 2))) outer_radius_num))
(define base_case (pow default_ratio 11))

(define (draw_one ratio)
  (if (< ratio base_case) 
      0
      (begin (set_data ratio)
           (rt 135)
           (fd (* ratio default_ratio (* inner_radius_num (sqrt 2))))
           (rt 90)
           (begin_fill)
           (color "black")
           (circle (* outer_radius_num default_ratio ratio default_ratio))
           (end_fill)
           (pu)
           (circle (* outer_radius_num default_ratio ratio default_ratio) 9)
           (pd)
           (rt 135)
           (draw_one (* ratio default_ratio)))
  )
)




(define columns 4)
(define rows 3)
(define (draw_bottom len num)
  (cond ((> len 0) (begin (pu) (goto (- (* (* 2 outer_radius_num) (- columns len)) (* 3 outer_radius_num)) 
                                     (- (* 3 outer_radius_num) (* (* 2 outer_radius_num) (- rows num)))) 
          (pd) 
          (seth 45) (draw_one 1) 
          (draw_bottom (- len 1) num)))
        ((> num 1) (draw_bottom columns (- num 1)))
        (else 0)))

(define columns2 3)
(define rows2 2)
(define (draw_top len num)
  (cond ((> len 0) (begin (pu) (goto (- (* (* 2 outer_radius_num) (- columns2 len)) (* 2 outer_radius_num)) 
                                     (- (* 2 outer_radius_num) (* (* 2 outer_radius_num) (- rows2 num)))) 
          (pd) 
          (seth 45) (draw_one 1) 
          (draw_top (- len 1) num)))
        ((> num 1) (draw_top columns2 (- num 1)))
        (else 0)))

(ht)
(draw_bottom columns rows)
(draw_top columns2 rows2)
(exitonclick)







