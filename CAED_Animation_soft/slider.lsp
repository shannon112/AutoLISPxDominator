;================================================
;   Slider.lsp : Slider crank mechanism
;   r: radius of the crank
;   th: angle of the crank in radian
;   c: length of the connecting rod,
;   e: offset of the slider
;   bx,by: length and width of the slider
;   w: width of connecting rod
;   di: inside diameter of the joint
;
;================================================
(defun c:slider (/ ip r c e w di bx by th)
  (graphscr)(osnapoff)
  (if (null rpoint) (load "util.lsp"))
  (setq ;r 30 th (dtr 30) c 100 e 10 bx 15 by 5
        r 30 th (dtr 30) c 70 e 5 bx 15 by 5
        w (/ r 10)  di (/ w 3.0)
        ip '(0 0)
  )
  (slider-crank ip r th c e w di bx by)
  (command "zoom" "e" "zoom" "0.5x")
  (princ)
)
;================================================
(defun slider-crank (ip r th c e w di bx by / s p1 p2 e1 e2 e3
                      e4 s1 s2 s3 par)
  (setq s (afunc r th c e)
        p1 (polar ip th r)   p2 (rpoint ip s e)
        e1 (entlast)
  )
  (command "ltscale" 0.5)
  (link ip p1 w di) (setq e2 (entlast))
  (link p1 p2 w di) (setq e3 (entlast))
  (box-center p2 bx by) (setq e4 (entlast))
  (setq s1 (sset e1 e2)   s2 (sset e2 e3)  s3 (sset e3 e4)
        par (list r th c e)
        sc_set (list ip s1 s2 s3 par) ;global variable
        sc_ang th                     ;global variable
  )
  (command "zoom" "e" "zoom" "0.5x");need zoom for dimensioning
  ;--- draw center lines
  (setq s (afunc r pi c e) s2 (afunc r 0.0 c e)
        p1 (rpoint ip s e) p2 (rpoint ip s2 e)
  )
  (command "layer" "m" "cen" "")
  (command "circle" ip r)
  (command "line" p1 p2 "")
  (setvar "dimcen" -3)
  (command "dimcenter" (polar ip (/ pi 4) r))
  (command "layer" "s" "0" "")
  (princ)
)
;================================================
(defun afunc (b th c e / s1 s2)
  (setq s1 (* b (cos th))
        s2 (- (* b (sin th)) e)
        s2 (- (* c c) (* s2 s2) )
        s2 (sqrt s2)
  )
  (+ s1 s2)
)
;================================================
(defun box-center (ip L w / p1 p2)
  (setq p1 (rpoint ip (/ L -2.0) (/ w -2.0))
        p2 (rpoint p1 L w)
  )
  (command "rectang" p1 p2)(princ)
)
;================================================
; rotate slider-crank
; sc_set : (<i.p.> sset_crank ss_link ss_slider par_list)
; par_list: (r th c e)
; sc_ang: crank angle in radian
; global: sc_set, sc_ang (in radian)
(defun c:rs (/ ip e1 e2 e3 par r th c e dth
               p1 p2 L1 L2 q1 q2 da ainc key)
  (graphscr)
  (setq ip (car sc_set)      e1 (nth 1 sc_set)
        e2 (nth 2 sc_set)    e3 (nth 3 sc_set)
        par (nth 4 sc_set)   r (car par)
        th (nth 1 par) c (nth 2 par) e (nth 3 par)
        ainc 10 ;angle increment in degree
  )
  (setq p1 (polar ip sc_ang r) L1 (afunc r sc_ang c e)
        p2 (rpoint ip L1 e)
  )
  (princ "\n Press <SPACE> to rotate clockwise")
  (princ ", Press <backspace> to rotate counter clockwise")
  (princ ", Press <RETURN> to end !")
  (setvar "cmdecho" 0)
  (while (and (setq key (cadr (grread))) (/= 13 key) )
    (if (= key 8)  ;<back space> is pressed
        (setq dth ainc)
        (setq dth (- ainc))
    )
    (setq sc_ang (+ sc_ang (dtr dth))
          q1 (polar ip sc_ang r) L2 (afunc r sc_ang c e)
          q2 (rpoint ip L2 e)
          da (- (angle q2 q1) (angle p2 p1))
          da (rtd da)
    )
    (command "rotate" e1 "" ip dth)
    (command "move" e2 e3 "" p2 q2)
    (command "rotate" e2 "" q2 da)
    (setq p1 q1 p2 q2)  ;update p1, p2
  );end-while
)
;==========================================================
(defun c:link (/ ip pe w w2 di)
  (graphscr)(osnapoff)
  (if (and
       (setq ip (getpoint "\nInput link insertion point <exit>: "))
       (setq pe (getpoint ip "\nInput link end point <exit>: "))
      )
      (progn
        (setq w2 (/ (distance ip pe) 10.0)
              str "\nInput link width <"
              str (strcat str (rtos w2) ">: ")
        )
        (setq w (getdist ip str)      w (if w w w2)
              w2 (/ w 4.0)
              str (strcat "\nInput joint hole diameter <" (rtos w2) ">: ")
              di (getdist ip str) di (if di di w2)
        )
        (link ip pe w di)
      )
  );end-if
  (princ)
)
;==========================================================
(defun link (ip pe w di / ri p1 p2 p3 p4 th th1 th2 pi/2 w/2)
  (setq th (angle ip pe)  pi/2 (/ pi 2.0) w/2 (/ w 2.0)
        th1 (+ th pi/2)   th2 (- th pi/2) ri (/ di 2.0)
        p1 (polar ip th1 w/2)  p2 (polar ip th2 w/2)
        p3 (polar pe th1 w/2)  p4 (polar pe th2 w/2)
  )
  (command "pline" p1 "a" "ce" ip p2 "L" p4 "a" "ce" pe p3 "L" "c")
  (command "circle" ip ri)  (command "circle" pe ri)
  (princ)
)
;==========================================================