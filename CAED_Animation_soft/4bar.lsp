;4-bar linkage
;4bar-TA.lsp
;2015/06/05
;TA-Wade

(if (null pi/2) (load "util-ta-wade.lsp"))

(defun c:4b ( / ip a b c d th w di ip)
  (graphscr)(osnapoff) (command "cmdecho" 0)
  (setq ;a 100 b 30 c 60 d 80 th 30
        a 100 b 30 c 60 d 50 th 60
	;a 100 b 30 c 60 d 80 th 60
	;a 100 b 30 c 60 d 70 th 60
	th (dtr th) w (/ b 10) di (/ w 3.0) ip '(0 0)
  )

  (4bar ip a b c d th w di)
  (command "zoom" "e" "zoom" "0.6x")
  (princ)
)

;==============================4bar==============================

(defun 4bar (ip a b c d th w di / p1 p2 p3 e1 e2 e3 e4 s1 s2 s3 par)
  (setq p1 (polar ip th b) p3 (polar ip 0.0 a)
	p2 (4barpoint ip a b c d th) e1 (entlast)
  )
  (command "ltscale" 0.5)
  (link ip p1 w di) (setq e2 (entlast))
  (link p1 p2 w di) (setq e3 (entlast))
  (link p3 p2 w di) (setq e4 (entlast))
  (setq s1 (sset e1 e2) s2 (sset e2 e3) s3 (sset e3 e4)
	par (list a b c d th)
	4b_set (list ip s1 s2 s3 par)
	4b_ang th
  )
  (command "zoom" "e" "zoom" "0.6x")

  ;draw center lines
  (command "layer" "s" "cen" "")
  (command "circle" ip b)
  (command "circle" p3 d)
  (command "line" ip p3 "")
  (command "layer" "s" "0" "")
  (princ)
)

;==============================4barpoint==============================

(defun 4barpoint (ip a b c d th / p1 p2 p3 e the alfa th3)
  (setq p1 (polar ip th b) p3 (polar ip 0.0 a)
	e (distance p1 p3) the (angle p1 p3)
	alfa (acos (/ (- (+ (* c c) (* e e)) (* d d)) (* 2 e c )))
	th3 (+ alfa the) p2 (polar p1 th3 c)
  )
  p2  ;return value

)

;==============================link==============================

(defun link (ip pe w di / r1 p1 p2 p3 p4 th th1 th2 w/2)
  (setq th (angle ip pe) w/2 (/ w 2.0)
	th1 (+ th pi/2) th2 (- th pi/2) ri (/ di 2.0)
	p1 (polar ip th1 w/2) p2 (polar ip th2 w/2)
	p3 (polar pe th1 w/2) p4 (polar pe th2 w/2)
  )
  (command "pline" p1 "a" "ce" ip p2 "l" p4 "a" "ce" pe p3 "l" "c")
  (command "circle" ip ri)
  (command "circle" pe ri)
  (princ)
)


;==============================r4b==============================
;rotate 4-bar linkage
(defun c:r4b ( / ip e1 e2 e3 par a b c d th dth tmp thmax thmin p1 p2 p3
	      L1 L2 q1 q2 da3 da4 ainc key flag)
  (graphscr) (setvar "cmdecho" 0)
  (setq ip (car 4b_set) e1 (nth 1 4b_set) e2 (nth 2 4b_set)
	e3 (nth 3 4b_set) par (nth 4 4b_set) a (car par)
	b (nth 1 par) c (nth 2 par) d (nth 3 par)
	ainc 10  ;angle increment in degree
	p1 (polar ip 4b_ang b) p3 (polar ip 0.0 a)
	p2 (4barpoint ip a b c d 4b_ang)
  )

  (if (>= (+ c d) (+ a b))
    (setq flag nil)
    (progn
      (setq flag t tmp (+ c d) tmp (* tmp tmp)
	    tmp (- (+ (* a a) (* b b)) tmp)
	    tmp (/ tmp 2.0 a b)
	    thmax (acos tmp) thmin (- thmax)
      )
    )
  )	;end if

  (princ "\n==============================")
  (princ "\nPress <SPACE> to rotate clockwise,")
  (princ "\nPress <backspace> to rotate counter clockwise,")
  (princ "\nPress <ENTER> to end!")
  (princ "\n==============================\n")

  (while (and (setq key (cadr (grread))) (/= 13 key) )
    (if (= key 8)
      (setq dth ainc) (setq dth (- ainc))
    )
    (if (and flag (> (+ 4b_ang (dtr dth)) thmax))
      (progn
	(setq dth (- thmax 4b_ang) dth (rtd dth))
      )
    )
    (if (and flag (< (+ 4b_ang (dtr dth)) thmin))
      (progn
	(setq dth (- thmin 4b_ang) dth (rtd dth))
      )
    )

    (setq 4b_ang (+ 4b_ang (dtr dth))
	  q1 (polar ip 4b_ang b) q2 (4barpoint ip a b c d 4b_ang)
	  da3 (- (angle q2 q1) (angle p2 p1)) da3 (rtd da3)
	  da4 (- (angle p3 q2) (angle p3 p2)) da4 (rtd da4)
    )

    (command "rotate" e1 "" ip dth) (command "rotate" e3 "" p3 da4)
    (command "move" e2 "" p2 q2) (command "rotate" e2 "" q2 da3)
    (setq p1 q1 p2 q2)  ;update p1 & p2
  )	;end while
)

  