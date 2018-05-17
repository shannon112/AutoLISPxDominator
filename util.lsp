;==========================================================
;   USER.LSP (UTIL.LSP) --- Utility Functions
;
;   Programmer : Tien-Tung Chung, Feb. 25, 1996
;   Functions :
;
;  Add function (sset2 e1 e2 ss)
;      --> Add entities after e1 until e2 to an exist set ss
;    c:ee --- edit PE2 files
;    c:ll --- Load Lisp files
;    c:cline --- center lines of a circle
;    c:lac : layer change, change layer of an entity
;    (xmove pt dx)
;    (ymove pt dy)
;    (xymove pt dx dy)
;    (rpoint pt dx dy)
;    (tr ip p0 r1 r2)
;    (tm ip p0 r1 r2)
;    (tu ip p0 r1 r2)
;    (2tl ip p0 r1 r2)
;    (pr ip p0 r)
;    (pm ip p0 r)
;    (pu ip p0 r)
;    (2pl ip p0 r)
;    (2rc ip p2 radius)  ;--- 2 rounded corner,  89/11/23 added
;--- add 89/11/23, used in LRING.LSP
;    vline : vertical line,
;            (vline ip p2 rad) or (vline ip angle rad)
;    rfillet : rod fillet, (rfillet ip p2 rad)
;    prf : parallel rod with fillets, (prf ip p2 rad rf)
;    c:prf() : interactine command to call prf().
;
;    c:ls() ;--- layer set by select an entity or keyin
;    (layer_f layername)    ;--- freeze
;    (layer_t layername)    ;--- thaw
;    (layer_s layername)
;    (layer_m layername)
;    (layer_c layername color)
;    (layer_l layername linetype)
;    (rotate ip e1 e2 ang)
;    (finddat fname value)
;    (iline ip ang r1 r2)
;    c:metric() ;--- set alyers, colors, linetypes for the metric system
;    (metric)
;--- add 2003/05/25
;    (osnapon)  ;turn on object snap mode
;    (osnapoff)  ;turn off object snap mode
;    (eraselayer <layer_name>) ; erase layer   2003/07/17
;--- Add 2004/02/28
;    (c:cc) : change color for entities
;    (ccolor entitis_set color_number) : change entities color
;--- Add 2004/03/02
;    (inters-c pc r p1 p2) : intersection point of a circle and a line
;    (c:ic) : interactive command for inters-c() functions
;    (changecolor en cno) : change color
;    (c:cc) : interactively change color
;--- 2016/01/15
;    (box_bc ip L W H) ; box with buttom center point ip
;
;==========================================================
(setq pi/2 (/ pi 2.0) pi/3 (/ pi 3.0) pi/4 (/ pi 4.0) pi/6 (/ pi 6.0))
;==========================================================
(defun line (p1 p2 / ) (command "line" p1 p2 "")(princ))
(defun c:rg (/) (command "regen")(princ))
(defun c:l0( / ) (layer_m "0"))
(defun c:lcen( / ) (layer_m "cen"))
(defun c:ldim( / ) (layer_m "dim"))
(defun c:ldash( / ) (layer_m "dash"))
(defun c:ls ( / )(command "layer" "s" pause "")(princ))
(defun c:lm ( / )(command "layer" "m" pause "")(princ))
(defun layer_f (ln / )(command "layer" "f" ln "")(princ))
(defun layer_t (ln / )(command "layer" "t" ln "")(princ))
(defun layer_s (ln / )(command "layer" "s" ln "")(princ))
(defun layer_m (ln / )(command "layer" "m" ln "")(princ))
(defun layer_c (c ln / )(command "layer" "c" c ln "")(princ))
(defun layer_l (l ln / )(command "layer" "L" l ln "")(princ))
(defun tan (ang) (/ (sin ang) (cos ang)))
(defun tand (ang) (setq ang (dtr ang)) (tan ang)  )
(defun sind (ang) (setq ang (dtr ang)) (sin ang)  )
(defun cosd (ang) (setq ang (dtr ang)) (cos ang)  )
(defun acos (x / y)(setq  y (sqrt (- 1.0 (* x x))))(atan y x))
(defun asin (y / x)(setq  x (sqrt (- 1.0 (* y y))))(atan y x))
(defun rtd( r / ) (* 180.0 (/ r pi)) )
(defun dtr( d / ) (* pi (/ d 180.0)) )
(defun d2r (d / )  (* pi (/ d 180.)) )
(defun r2d (r / )  (* 180. (/ r pi)) )
;================================================
(defun c:cc (/ e1 cno )
  (graphscr)
  (if (and
        (setq e1 (car (entsel "\n Select an entity <exit>:")))
        (setq cno (acad_colordlg 0 t))
      )
      (progn
        (command "change" e1 "" "P" "C" cno "")
      )
  )
  (princ)
)
;================================================
(defun ccolor (en cno /)
  (command "change" en "" "P" "C" cno "")(princ)
)
;==========================================================
(setq dotsize 0.4)
(defun c:dot( / pt os s1 a)
  (initget "Diameter")
  ;(setq os (getvar "osmode"))
  ;(command "osnap" "int,mid,end,quad")
  (setq pt (getpoint "\nCenter point of the dot /Diameter <exit>: "))
  (if dotsize t (setq dotsize 0.4))
  (if (= pt "Diameter")
    (progn
      (setq s1 (strcat "\nNew dot size <" (rtos dotsize) ">: "))
      (if (setq a (getdist s1)) (setq dotsize a))
    )
    (command "circle" pt dotsize)
  )
  ;(setvar "osmode" os)
  (redraw)(princ)
)
;==========================================================
;   lac --- layer change, change layer of an entity
(defun c:lac ( / ln en ed)
  (setq ln (getstring "\nChange to layer name <exit>: "))
  (if (/= ln "")
    (progn
      (while (setq en (entsel "\nSelect an entity <exit>: "))
             (setq en (car en)
                   ed (entget en)
                   ed (subst (cons 8 ln) (assoc 8 ed) ed)
             )
             (entmod ed)
      );end-while
    )
  );end-if
  (redraw)(princ)
)
;==========================================================
;--- draw a center line on layer "cen"
(defun c:cl( / ln)
  (setq ln (getvar "clayer"))
  (layer_m "cen") (command "line" pause pause "")
  (layer_s ln)(redraw)(princ)
)
;==========================================================
;--- draw a dash line on layer "dash"
(defun c:dl( / ln)
  (setq ln (getvar "clayer"))
  (layer_m "dash") (command "line" pause pause "")
  (layer_s ln)(redraw)(princ)
)
;==========================================================
;--- move a point in x-dir.
(defun xmove (pt dx / x)
  (setq x (car pt) pt (cdr pt))
  (cons (+ x dx) pt)
)
;--- move a point in y-dir.
(defun ymove (pt dy / x y)
  (setq x (car pt) y (cadr pt) pt (cddr pt))
  (cons x (cons (+ y dy) pt))
)
;--- move a point in x and y dir.
(defun xymove (pt dx dy / x y)
  (setq x (car pt) y (cadr pt) pt (cddr pt))
  (cons (+ x dx) (cons (+ y dy) pt))
)
(defun rpoint (pt dx dy / x y)
  (setq x (car pt) y (cadr pt) pt (cddr pt))
  (cons (+ x dx) (cons (+ y dy) pt))
)
(defun rpoint3d (pt dx dy dz / x y z)
   (setq x (car pt) y (cadr pt) z (caddr pt)
         z (if z z 0.0)
         pt (list (+ x dx) (+ y dy) (+ z dz))
   )
   pt
)
;==========================================================
; Global varibal : lspfn - Lisp File Name
(defun c:ll ( / str fn)
  (setq str "\nLoad Lisp file <")
  (if lspfn
      (setq str (strcat str lspfn ".LSP>: "))
      (setq str (strcat str "exit>: "))
  );end-if
  (if (/= "" (setq fn (getstring str)))
      (setq lspfn (strcase fn))
  );end-if
  (if lspfn
      (progn
        (setq fn (strcat lspfn ".LSP"))
        (load fn)
        (princ (strcat "Lisp File - " fn " is loaded !"))
      )
  );end-if
  (princ)
);end_c:ll()
;==========================================================
; Global varibal : lspfn - Lisp File Name
(defun c:ee ( / str fn)
  (setq str "\nEdit Lisp file name <")
  (if lspfn
      (setq str (strcat str lspfn ".LSP>: "))
      (setq str (strcat str "exit>: "))
  );end-if
  (if (/= "" (setq fn (getstring str)))
      (setq lspfn (strcase fn))
  );end-if
  (if lspfn
      (progn
        (setq fn (strcat lspfn ".LSP"))
        (command "pe2" fn)
      )
  );end-if
  (princ)
);end_c:ee()
;==========================================================
;global variables :
(setq dimlayer "dim"   thinlayer "thin"
      dashlayer "dash" mainlayer "0"         ;"main"
      cenlayer "cen"
)
;--- load ISO linetype
(command "linetype" "L" "acad_iso10w100" "acadiso.lin" "")
(command)
(command "linetype" "L" "dashed2" "acadiso.lin" "")
(command)

(defun c:metric( / ) (metric))
(defun metric (/ )
   (layer_m dimlayer)        (layer_m thinlayer)
   ;(layer_m mainlayer)
   (layer_m cenlayer)        (layer_m dashlayer)
   ;--- set layer color
   (layer_c "green" thinlayer)   (layer_c "red" dimlayer)
   (layer_c "blue" cenlayer)     (layer_c "white" mainlayer)
   (layer_c "magenta" dashlayer)
   ;--- set layer line type
   (layer_l "acad_iso10w100" cenlayer)
   (layer_l "dashed2" dashlayer)
   ;(layer_l "center" cenlayer)
   ;(layer_l "hidden" dashlayer)
   ;--- scales
   ;(setvar "ltscale" 10.0)
   ;(setvar "dimscale" 10.0)
   (layer_s mainlayer)
   (princ)
);end-metric
;==========================================================
;  selset : selection set after e1 until e2
(defun selset (e1 e2 / ss )
  (setq e1 (if e1 (entnext e1) (entnext))
        ss (ssadd e1)
  )
  (while (not (equal e1 e2))
     (setq e1 (entnext e1)
           ss (ssadd e1 ss)
     )
  )
  ss  ;return value
);end-selset()
;==========================================================
;  Rotate all enelemts after e1 until e2
;  ang : angle in degree,
;        if ang is nil, it will be input by user interactively.
;
(defun rotate (ip e1 e2 ang / ss)
  (setq e1 (if e1 (entnext e1) (entnext))
        ss (ssadd e1)
  )
  (while (not (equal e1 e2))
     (setq e1 (entnext e1)
           ss (ssadd e1 ss)
     )
  )
  (if ang
      (command "rotate" ss "" ip ang)
      (command "rotate" ss "" ip pause)
  )
  (redraw)(princ)  ;null return
);end-rotate()
;==========================================================
; return a selection set after e1 until e2, add to exist set ss
(defun sset2 (e1 e2 ss / ss)
  (setq e1 (if e1 (entnext e1) (entnext))
        ss (ssadd e1 ss)  )
  (while (not (equal e1 e2))
     (setq e1 (entnext e1)  ss (ssadd e1 ss)   )
  )
  ss ;return value
)
;==========================================================
; return a selection set after e1 until e2
(defun sset (e1 e2 / ss)
  (setq e1 (if e1 (entnext e1) (entnext))
        ss (ssadd e1)  )
  (while (not (equal e1 e2))
     (setq e1 (entnext e1)  ss (ssadd e1 ss)   )
  )
  ss ;return value
)
;==========================================================
;   Relative point
;   Returns a relative point by giving dx and dy
;
(defun relpoint(pt dx dy /) (relpoint pt dx dy))
(defun relpoint(pt dx dy /)
  (polar (polar pt 0.0 dx) (/ pi 2.0) dy)
)
;==========================================================
;  global cross_len
(setq cross_len 3)
(defun c:cross ( / ip len ang str)
  (setq str (strcat "\nInput cross length <" (rtos cross_len) ">: "))
  (osnapon)

  (if (and (setq ip (getpoint "\nInput insertion point <exit> : "))
           (if (setq len (getdist str)) t (setq len cross_len))
           (if (setq ang (getangle ip "\nInput angle <0.0> : "))
               t (setq ang 0.0)
           )
      )
      (progn
        (osnapoff)
        (cross ip ang len)
        (setq cross_len len)
      )
  );end-if
  (princ)
)
;==========================================================
;--- draw a cross "X"
;    len = length of the hole
;
(defun cross (ip ang len / th1 th2 p1 p2 p3 p4 pi/4)
  (setq pi/4 (/ pi 4.0))
  (setq th1 (+ ang pi/4)      th2 (- ang pi/4)
        p1 (polar ip th1 len) p2 (polar ip th2 len)
        p3 (polar ip (+ th1 pi) len)
        p4 (polar ip (+ th2 pi) len)
  )
  (command "line" p1 p3 "")(command "line" p2 p4 "")
  (redraw)(princ)
);end-of-cross()
;==========================================================
;--- draw center lines for a circle
(defun c:cline(/ en ed et p0 r ang)
  (if (and (setq en (car (entsel "\nSelect a circle <exit> : ")))
           (setq ed (entget en) et (cdr (assoc 0 ed))  )
           (or (= "CIRCLE")(= "ARC"))
           (setq p0 (cdr (assoc 10 ed)))
           (setq r  (cdr (assoc 40 ed)))
           (if (setq ang (getangle "Input incline angle <0.0> : "))
               t (setq ang 0.0)
           )
      );end-if-test
      (progn (setq d (* r 2. 1.2)) (cline2 p0 ang d))
  );end-if
  (princ)
)
;==========================================================
;--- draw center lines for a circle
(defun cline(en ang / ed et p0 r d)
 (setq ed (entget en) et (cdr (assoc 0 ed))  )
 (if  (or (= et "CIRCLE")(= et "ARC"))
   (progn
     (setq ed (entget en)           p0 (cdr (assoc 10 ed))
           r  (cdr (assoc 40 ed))   d (* r 2. 1.2)
     )
     (cline2 p0 ang d)
   );end-if-then
   (progn
     (prtlist (list "\nNot a circle or arc -- " ed))(pexit)
   );end-if-else
 );end-if
 (redraw)(princ)
)
;==========================================================
;--- draw center lines for a circle
(defun cline2 (ip ang d / th1 th2 r cl p1 p2 p3 p4)
  (setq pi/2 (/ pi 2.0))
  (setq th1 (+ ang pi/2)     th2 (- ang pi/2)
        r (/ d 2.)           p1 (polar ip (+ ang pi) r)
        p2 (polar ip ang r)  p3 (polar ip th2 r)
        p4 (polar ip th1 r)
  )
  (setq cl (getvar "clayer")) (layer_s "cen")
  (command "line" p1 p2 "")(command "line" p3 p4 "")
  (layer_s cl)  (princ)
);end-of-cline()
;==========================================================
(defun c:phole ( / ip dia len ang)
  (if (and (setq ip (getpoint "\nInput insertion point <exit> : "))
           (setq dia (getdist ip "\nInput hole dia. <exit> : "))
           (setq len (getdist ip "\nInput hole length <exit> : "))
           (if (setq ang (getangle ip "\nInput angle <0.0> : "))
               t (setq ang 0.0)
           )
      )
      (phole ip ang dia len)
  );end-if
  (redraw)(princ)
)
;==========================================================
;--- draw the front view of a hole in a plate
;    d   = diameter or width
;    len = length of the hole
;
(defun phole (ip ang d len / th1 th2 r p1 p2 p3 p4)
  (setq th1 (+ ang pi/2) th2 (- ang pi/2) r (/ d 2.)
        p1 (polar ip th2 r)     p2 (polar p1 ang len)
        p3 (polar p1 th1 d)     p4 (polar p2 th1 d)
  )
  (command "line" p1 p2 "")(command "line" p3 p4 "")(princ)
);end-of-phole()
;==========================================================
;   Counterbored holes
;   ang= direction angle       d = dia.
;   h = counterbore depth
(defun c:cbore ( / ip dia len ang)
  (if (and (setq ip (getpoint "\nInput insertion point <exit> : "))
           (setq dia (getdist ip "\nInput hole dia. <exit> : "))
           (setq len (getdist ip "\nInput hole depth <exit> : "))
           (if (setq ang (getangle ip "\nInput angle <0.0> : "))
               t (setq ang 0.0)
           )
      )
      (cbore ip ang dia len)
  );end-if
  (princ)
)
;==========================================================
(defun cbore (ip ang d h / th1 th2 r p1 p2 p3 p4)
  (setq th1 (+ ang pi/2) th2 (- ang pi/2) r (/ d 2.)
        p1 (polar ip th1 r)   p2 (polar ip th2 r)
        p3 (polar p1 ang h)   p4 (polar p2 ang h)
  )
  (command "line" p1 p3 p4 p2 "")(redraw)(princ)
)
;==========================================================
;   Drilled holes
;   ang= direction angle
;   d = drill dia.
;   h = drill depth
;   needs : dhole()
(defun c:dhole (/ ip ang d h)
  (graphscr)
  (if (and (setq ip (getpoint "\nInput insertion point <exit> : "))
           (if (setq ang (getangle ip "\nInput angle <0.0> : "))
               t (setq ang 0.0))
           (setq d (getdist ip "\nInput diameter of the hole <exit> : "))
           (setq h (getdist ip "\nInput depth of the hole <exit> : "))
      )
      (progn
        (dhole ip ang d h)
      )
  );end-if
  (redraw)(princ)
);end-c:dhole()
;==========================================================
;   Drilled holes
;   ang= direction angle
;   d = drill dia.
;   h = drill depth
;
;    needs : pi, pi/2, pi/6
;
(defun dhole (ip ang d h / th1 th2 r p0 p1 p2 p3 p4)
  (setq th1 (+ ang pi/2) th2 (- ang pi/2) r (/ d 2.0)
        p0 ip
        p1 (polar p0 th1 r)    p2 (polar p0 th2 r)
        p0 (polar p0 ang h)
        p3 (polar p0 th1 r)    p4 (polar p0 th2 r)
        p5 (polar p0 ang (* r (tan pi/6))  )
  )
  (command "line" p1 p3 p4 p2 "")(command "line" p3 p5 p4 "")
  (princ)
);end_dhole()
;==========================================================
;   Find a data list in file "fn" with a given first element "di"
;
;   If succeed, returns the data list, otherwize, returns a nil.
;
(defun finddat(fn di / fp dat d char)
  (setq fp (open fn "r"))
  (while (and (setq dat (read-line fp))
              ;--- test if a comment card
              (setq char (substr dat 1 1))
              (if (or (= char ";") (= dat ""))
                  t
                  (progn
                     (setq dat (read dat) d (car dat))
                     (/= d di)
                  )
              );end-if
         );end-while-test
  );end-while
  (close fp)
  (if (= d di)
      dat        ;if-then
      (progn     ;if-else
       (princ "\nData list with value - ")(princ di)
       (princ " not found in file - ") (princ fn)
       ;(pexit)
       nil
      )
  );end-if
);end-finddat()
;==========================================================
;   Rectangles with ip in bottom center
;
(defun rect_bc (ip ang w h / p1 p2 p3 p4)
  (setq p1 (polar ip (+ pi ang) (/ w 2.))
        p2 (polar p1 ang w)
        p3 (polar p1 (+ ang pi/2) h)
        p4 (polar p3 ang w)
  )
  (command "line" p1 p2 p4 p3 "c")
  (princ)
);end_of_rect_bc()
;==========================================================
;   Rectangles with ip in middle center
;
(defun rect_mc (ip w h ang / p1 p2 p3 p4)
  (setq p1 (relpoint ip (/ w -2.) (/ h -2.))
        p2 (polar p1 ang w)
        p3 (polar p1 (+ ang pi/2) h)
        p4 (polar p3 ang w)
  )
  (command "line" p1 p2 p4 p3 "c")
  (princ)
);end_of_rect_mc()
;==========================================================
;   Rectangles with ip in left corner
;
(defun rect_lc (ip w h ang / p1 p2 p3 p4)
  (setq p1 ip
        p2 (polar p1 ang w)
        p3 (polar p1 (+ ang pi/2) h)
        p4 (polar p3 ang w)
  )
  (command "line" p1 p2 p4 p3 "c")
  (princ)
);end_of_rect_lc()
;==========================================================
;   Rectangles with ip in right corner
;
(defun rect_rc (ip w h ang / p1 p2 p3 p4)
  (setq p1 (polar ip pi w)
        p2 ip
        p3 (polar p1 (+ ang pi/2) h)
        p4 (polar p3 ang w)
  )
  (command "line" p1 p2 p4 p3 "c")
  (princ)
);end_of_rect_rc()
;==========================================================
(defun rod (ip ang d h / th1 th2 r p1 p2 p3 p4)
  (setq
        th1 (+ ang pi/2)
        th2 (- ang pi/2)
        r (/ d 2.)
        p1 (polar ip th1 r)
        p2 (polar ip th2 r)
        ip (polar ip ang h)
        p3 (polar ip th1 r)
        p4 (polar ip th2 r)
  )
  (command "line" p1 p3 p4 p2 "c")
  (princ)
)
;==========================================================
;--- draw a line with given an intermidiate point and two lengths
(defun c:iline ( / ip ang l1 l2)
  (if (and (setq ip (getpoint "\nInput insertion point <exit> : "))
           (setq l1 (getdist ip "\nInput 1st length L1 <exit> "))
           (if (setq l2 (getdist ip "\nInput 2nd length L2 <L1> : "))
               t (setq l2 l1)
           )
           (if (setq ang (getangle ip "\nInput angle <0.0> : "))
               t (setq ang 0.0)
           )
      )
      (iline ip ang l1 l2)
  );end-if
  (princ)
)
;==========================================================
(defun iline (ip ang l1 l2 / th1 th2 p1 p2 p3 p4)
  (setq
        p1 (polar ip (+ ang pi) l1)
        p2 (polar ip ang l2)
  )
  (command "line" p1 p2 "")
  (princ)
);end-of-iline()
;==========================================================
(defun c:box ( / ip pc)
  (if (and (setq ip (getpoint "\nInput insertion point <exit> : "))
           (setq pc (getcorner ip "\Input corner point <exit> "))
      )
      (box ip pc)
  );end-if
  (princ)
)
;==========================================================
;
(defun box (ip pc / dx dy p1 p2 p3 p4)
  (setq
        dx (- (car pc) (car ip))
        dy (- (cadr pc) (cadr ip))
        p1 (polar ip 0.0 dx)
        p2 (polar ip pi/2 dy)
  )
  (command "line" ip p1 pc p2 "c")
  (princ)
);end-of-box()
;==========================================================
;==========================================================
;   Tapered holes
;   ang= direction angle
;   d = drill dia.
;   h = drill depth
;
;    needs : thole()
;
(defun c:thole (/ ip ang d1 d2 l p0)
  (graphscr)
  (if (and (setq ip (getpoint "\nInput insertion point <exit> : "))
           (if (setq ang (getangle ip "\nInput angle <0.0> : "))
               t (setq ang 0.0))
           (setq d1 (getdist ip "\nInput 1st diameter <exit> : "))
           (setq l (getdist ip "\nInput depth of the hole <exit> : "))
           (setq p0 (polar ip ang l))
           (setq d2 (getdist p0 "\nInput 2nd diameter <exit> : "))
      )
      (progn
        (thole ip ang d1 d2 l)
      )
  );end-if
  (redraw)(princ)
);end-c:thole()
;==========================================================
;   Tapered holes
;   ang= direction angle
;   d1 = 1st drill dia.
;   d2 = 2nd drill dia.
;   l = drill length
;
;    needs : pi, pi/2
;
(defun thole (ip ang d1 d2 h / th1 th2 r1 r2 p0 p1 p2 p3 p4)
  (setq th1 (+ ang pi/2)
        th2 (- ang pi/2)
        r1 (/ d1 2.0)
        r2 (/ d2 2.0)
        p0 ip
        p1 (polar p0 th1 r1)
        p2 (polar p0 th2 r1)
        p0 (polar p0 ang h)
        p3 (polar p0 th1 r2)
        p4 (polar p0 th2 r2)
  )
  (command "line" p1 p3 p4 p2 "")
  (princ)
);end_thole()
;==========================================================
(defun prtlist( dl / e)
  (if (listp dl)
      (progn
        (princ "\n(")
        (foreach el dl (princ "\n ")(princ el))
        (princ "\n)")
      )
      (princ dl)  ;--- if-else
  );end-if
  (princ)
)
;==========================================================
(defun pexit ( / )
  (princ "\nPress any key to continue !")(grread)
  (command)
)
;==========================================================
(defun pcontinue ( / )
  (princ "\nPress any key to continue !")(grread)
  ;(command)
)
;==========================================================
;    c:ls() ;--- layer set by select an entity or keyin
(defun c:ls ( / ln)
  (setq ln (getln))  (if (and ln (/= ln ""))(layer_s ln))  (princ)
)
(defun c:lm ( / ln)  ;--- layer make
  (setq ln (getln))  (if (and ln (/= ln ""))(layer_m ln))  (princ)
)
(defun c:lf ( / ln)  ;--- layer freeze
  (setq ln (getln))  (if (and ln (/= ln ""))(layer_f ln))  (princ)
)
(defun c:lt ( / ln)  ;--- layer thaw
  (setq ln (getln))  (if (and ln (/= ln ""))(layer_t ln))  (princ)
)
;==========================================================
(defun getln ( / ln ed )
  (graphscr)
  ;(princ "\Set to a new current layer !")
  (initget "Keyin")
  (setq ln (entsel "\nSelect an entity to get layer name /Keyin <exit>: "))
  (if (= ln "Keyin")
      (progn
        (setq ln (getstring "\nInput the layer name <exit>: "))
      )
      (progn
        (setq ed (entget (car ln))
              ln (cdr (assoc 8 ed))
        )
      )
  )
  ln ;return value
);end-getln()
;================================================
;   TROD.LSP --- Taper Rod
;   Functions :
;      tr : tapered rod
;      tu : tapered U shape
;      tm : tapered M shape
;      2tl : 2 tapered lines
;      pr : parallel rod
;      pu : parallel U shape
;      pm : parallel M shape
;      2pl : 2 parallel lines
;      2vl : draw 2 vertical line
;
;   Programmer : Tien-Tung Chung,  Jan. 10, 1997
;================================================
(defun tr (ip p0 r1 r2 / ) (tr0 ip p0 r1 r2 t t t)(princ))
(defun tu (ip p0 r1 r2 / ) (tr0 ip p0 r1 r2 t t nil)(princ))
(defun tm (ip p0 r1 r2 / ) (tr0 ip p0 r1 r2 nil t t)(princ))
(defun 2tl (ip p0 r1 r2 / ) (tr0 ip p0 r1 r2 nil t nil)(princ))
(defun pr (ip p0 r / ) (tr0 ip p0 r r t t t)(princ))
(defun pu (ip p0 r / ) (tr0 ip p0 r r t t nil)(princ))
(defun pm (ip p0 r / ) (tr0 ip p0 r r nil t t)(princ))
(defun 2pl (ip p0 r / ) (tr0 ip p0 r r nil t nil)(princ))
;================================================
;-- draw 2 vertical lines
(defun 2vl (ip p0 r1 r2 / p2 p3 p4 p5 a1 a2)
  (setq a1 (angle ip p0) a1 (+ a1 (/ pi 2.0)) a2 (+ a1 pi)
        p2 (polar ip a1 r1) p3 (polar ip a1 r2)
        p4 (polar ip a2 r1) p5 (polar ip a2 r2)
  )
  (if (> (abs (- r2 r1)) 0.00001)
      (progn
        (command "line" p2 p3 "" "line" p4 p5 "")
      )
  )
  (princ)
)
;================================================
(defun c:trod ( / ) (c:tr) (princ))
(defun c:tr ( / ip p0 r1 r2)
  (graphscr)
  (if (and (setq ip (getpoint "\nInput insertion point <exit>: "))
           (setq p0 (getpoint ip "\nInput the end point <exit>: "))
           (setq r1 (getdist ip "\nInput first radius <exit>: "))
           (setq r2 (getdist p0 "\nInput second radius <exit>: "))
      )
      (progn (tr0 ip p0 r1 r2 t t t) )
  )
  (redraw)  (princ)
)
;================================================
;--- parallel U shape
(defun c:pu ( / ip p0 r)
  (graphscr)
  (if (and (setq ip (getpoint "\nInput insertion point <exit>: "))
           (setq p0 (getpoint ip "\nInput the end point <exit>: "))
           (setq r (getdist ip "\nInput radius <exit>: "))
      )
      (progn (pu ip p0 r) )
  )
  (redraw)  (princ)
)
;================================================
;--- parallel M shape
(defun c:pm ( / ip p0 r)
  (graphscr)
  (if (and (setq ip (getpoint "\nInput insertion point <exit>: "))
           (setq p0 (getpoint ip "\nInput the end point <exit>: "))
           (setq r (getdist ip "\nInput radius <exit>: "))
      )
      (progn (pm ip p0 r) )
  )
  (redraw)  (princ)
)
;================================================
;--- 2 parallel lines
(defun c:2pl ( / ip p0 r)
  (graphscr)
  (if (and (setq ip (getpoint "\nInput insertion point <exit>: "))
           (setq p0 (getpoint ip "\nInput the end point <exit>: "))
           (setq r (getdist ip "\nInput radius <exit>: "))
      )
      (progn (2pl ip p0 r) )
  )
  (redraw)  (princ)
)
;================================================
;=== parallel rod
(defun c:pr ( / ip p0 r)
  (graphscr)
  (if (and (setq ip (getpoint "\nInput insertion point <exit>: "))
           (setq p0 (getpoint ip "\nInput the end point <exit>: "))
           (setq r (getdist ip "\nInput radius <exit>: "))
      )
      (progn (pr ip p0 r) )
  )
  (redraw)  (princ)
)
;================================================
;--- 2 Taper Lines
(defun c:2tl ( / ip p0 r1 r2)
  (graphscr)
  (if (and (setq ip (getpoint "\nInput insertion point <exit>: "))
           (setq p0 (getpoint ip "\nInput the end point <exit>: "))
           (setq r1 (getdist ip "\nInput first radius <exit>: "))
           (setq r2 (getdist p0 "\nInput second radius <exit>: "))
      )
      (progn (2tl ip p0 r1 r2) )
  )
  (redraw)  (princ)
)
;================================================
(defun c:tm ( / ip p0 r1 r2)
  (graphscr)
  (if (and (setq ip (getpoint "\nInput insertion point <exit>: "))
           (setq p0 (getpoint ip "\nInput the end point <exit>: "))
           (setq r1 (getdist ip "\nInput first radius <exit>: "))
           (setq r2 (getdist p0 "\nInput second radius <exit>: "))
      )
      (progn (tm ip p0 r1 r2) )
  )
  (redraw)  (princ)
)
;================================================
(defun c:tu ( / ip p0 r1 r2)
  (graphscr)
  (if (and (setq ip (getpoint "\nInput insertion point <exit>: "))
           (setq p0 (getpoint ip "\nInput the end point <exit>: "))
           (setq r1 (getdist ip "\nInput first radius <exit>: "))
           (setq r2 (getdist p0 "\nInput second radius <exit>: "))
      )
      (progn (tu ip p0 r1 r2) )
  )
  (redraw)  (princ)
)
;================================================
(defun tr0 (ip p0 r1 r2 f1 f2 f3 / p1 p2 p3 p4 th1 th2 th3)
  (setq
        th1 (angle ip p0)
        th2 (+ th1 (/ pi 2.0))
        th3 (- th2 pi)
        p1 (polar ip th2 r1)
        p2 (polar ip th3 r1)
        p3 (polar p0 th2 r2)
        p4 (polar p0 th3 r2)
  )
  (if (and (not (equal r1 0.0 0.0001)) f1)
      (progn
        (command "line" p1 p2 "")
      )
  )
  (if (and (not (equal (distance p1 p3) 0.0 0.0001)) f2)
      (progn
        (command "line" p1 p3 "")
        (command "line" p2 p4 "")
      )
  )
  (if (and (not (equal r2 0.0 0.0001)) f3)
      (progn
        (command "line" p3 p4 "")
      )
  )
  (princ)
)
;================================================
(defun 2rc (ip p2 r / p3 p4 p5 p6 rf ang th1 th2)
  (setq rf (distance ip p2)
        ang (angle ip p2)
        th1 (+ ang (/ pi 2.0))
        th2 (- ang (/ pi 2.0))
        p3 (polar ip th1 (+ r rf))
        p6 (polar ip th2 (+ r rf))
        p4 (polar p2 th1 r)
        p5 (polar p2 th2 r)
  )
  (command "arc" p3 "E" p4 "R" rf)
  (command "arc" p5 "E" p6 "R" rf)
  (princ)
);end-2rc()
;================================================
;--- ang can be an angle or a point
(defun vline (ip ang r / th1 th2 p1 p2)
  (if (listp ang)(setq ang (angle ip ang)))
  (setq th1 (+ ang (/ pi 2.0))
        th2 (- ang (/ pi 2.0))
        p1 (polar ip th1 r)
        p2 (polar ip th2 r)
  )
  (command "line" p1 p2 "")
  (princ)
)
;================================================
;--- rod fillet
(defun rfillet (ip p2 r / rf ang r2 th1 th2 p3 p4 p5 p6 )
  (setq ang (angle ip p2)
        rf (distance ip p2)
        th1 (+ ang (/ pi 2.0))
        th2 (- ang (/ pi 2.0))
        p3 (polar ip th1 (- r rf))
        p5 (polar ip th2 (- r rf))
        p4 (polar p2 th1 r)
        p6 (polar p2 th2 r)
  )
  (command "arc" p4 "E" p3 "R" rf)
  (command "arc" p5 "E" p6 "R" rf)
  (princ)
);end-rfillet()
;================================================
(defun prf (ip pe r rf /  ang len p2 p3 )
  (setq ang (angle ip pe)
        len (distance ip pe)
        p2 (polar ip ang rf)
        p3 (polar ip ang (- len rf))
  )
  (vline ip p2 (- r rf))
  (rfillet ip p2 r)
  (2pl p2 p3 r)
  (rfillet pe p3 r)
  (vline pe ang (- r rf))
  (redraw)(princ)
);end-prf()
;================================================
(defun c:prf (/ ip pe r rf)
  (graphscr)
  (if (and (setq ip (getpoint "\nInput the insertion point <exit>: "))
           (setq pe (getpoint ip "\nInput the end point <exit>: "))
           (setq r (getdist ip "\nInput the rod radius <exit>: "))
           (setq rf (getdist ip "\nInput the fillet radius <exit>: "))
      )
      (princ
        (prf ip pe r rf)
      )
  );end-if
  (princ)
);end-c:prf()
;================================================
;======================================
;   LAYER.LSP --- Set up layers : dash, cen, dim, frame
;
;   Programmer : Tien-Tung Chung, May 29, 1999
;======================================
(command "linetype" "L" "acad_iso10w100" "acadiso.lin" "")
(command)
(command "linetype" "L" "dashed2" "acadiso.lin" "")
(command)
(command "layer" "m" "cen" "Color" "Magenta" "" "Ltype" "acad_iso10w100" "" "")
(command "layer" "m" "dash" "Color" "Red" "" "Ltype" "dashed2" "" "")
(command "layer" "m" "dim" "Color" "Blue" "" "Ltype" "continuous" "" "")
(command "layer" "m" "frame" "Color" "Green" "" "Ltype" "continuous" "" "")
(command "layer" "s" "0" "")
;--- set styles
(command "style" "greekc" "greekc" "" "" "" "" "" "")
(command "style" "italic" "italic" "" "" "" "" "" "")
(command "style" "simplex" "simplex" "" "" "" "" "" "")
(command "style" "complex" "complex" "" "" "" "" "" "")
(command "viewres" "" 1000)
(command "facetres" 10)
(command "dim" "sty" "complex" "dimexo" 1.5)(command)
(princ)  ;null return value
;================================================
; turn on object snap
(defun osnapon (/ k)
  (setq k (getvar "osmode"))
  (if (>= k 16384)
      (progn
        (setq k (- k 16384))(setvar "osmode" k)
      )
  )(princ)
)
;================================================
; turn off object snap
(defun osnapoff (/ k)
  (setq k (getvar "osmode"))
  (if (< k 16384)
      (progn
        (setq k (+ k 16384))(setvar "osmode" k)
      )
  )(princ)
)
;================================================
(defun eraselayer (LN / ss)
  (setq ss (ssget "X" (list (cons 8 LN)))  )
  (if ss (command "erase" ss "") )  (princ)
)
;================================================
;   INTERS-C.LSP
;      Find the intersection point of a circle and  line
;
;   Programmer: Tien-Tung Chung, 2004/03/02
;================================================
(defun c:ic (/ pc r p1 p2 p3)
  (graphscr)(setvar "pdmode" 3)(osnapoff)
  (if (and
        (setq pc (getpoint "\nInput the center point <exit>: "))
        (setq r (getdist pc "\nInput the radius <exit>: "))
        (null (command "circle" pc r))
        (setq p1 (getpoint "\nInput the 1st point <exit>: "))
        (setq p2 (getpoint p1 "\nInput the 2nd point <exit>: "))
      )
      (progn
        (command "line" p1 p2 "")
        (setq p3 (inters-c pc r p1 p2))
        (command "POINT" p3)
      )
  )
  (princ)
)
;================================================
; find intersection point of a circle and a line
(defun inters-c (pc r p1 p2 / p3 L1 a b c alfa beta th1 pi/2 d1 d2)
  (setq d1 (distance pc p1) d2 (distance pc p2))
  (if (> d1 d2) (setq p3 p1 p1 p2 p2 p3)) ; shift p1, p2
  (setq alfa (angle p1 p2)     beta (- (angle p1 pc) alfa)
        pi/2 (/ pi 2.0)        th1 (- beta pi/2)
        L1 (distance pc p1)
        b (* L1 (cos th1))     c (* L1 (sin th1))
        a (sqrt (- (* r r) (* b b)))
        p3 (polar p1 alfa (- a c))
  )
  p3 ;return value
)
;================================================
(defun changecolor (en cno / )
  (command "change" en "" "P" "C" cno "")(princ)
)
;================================================
(defun c:da () (command "erase" "all" "")(princ))
;================================================
; Global varibal : arxfn - ARX File Name
(defun c:aL ( / str fn)
  (setq str "\nLoad ARX file name <")
  (if arxfn
      (setq str (strcat str arxfn ">: "))
      (setq str (strcat str "exit>: "))
  );end-if
  (if (/= "" (setq fn (getstring str)))
      (setq arxfn (strcase fn))
  );end-if
  (if arxfn
      (progn
        (setq fn (strcat arxfn ".ARX"))
        (arxload fn)
	(princ (strcat "\*** Load " arxfn " ! ***"))
      )
  );end-if
  (princ)
);end_c:aL()
;==========================================================
; arx unload
; Global varibal : arxfn - ARX File Name
(defun c:aU (/ str fn)
  (if arxfn
      (progn
        (setq fn (strcat arxfn ".ARX"))
        ;(command "ARX" "U" arxfn)
	(arxunload arxfn)
	(princ (strcat "\*** Unload " arxfn " ! ***"))
      )
      (princ "\n*** Error, arxfn not given ! ***")
  );end-if
  (princ)
)
;==========================================================
;  p1 , p2 can be two points on each edge of the entities,
;  or, can be two entity names of the two edges
;
(defun fillet (r p1 p2 / )
  (command "fillet" "R" r)
  (command "fillet" p1 p2)
  (princ)
)
(defun pause (msg / )
  (princ msg)(princ "\n*** press return to continus !")
  (getstring)
  (princ)
)
;==========================================================
(defun princ2 (a b ) (princ a)(princ b))
(defun princ3 (a b c ) (princ a)(princ b)(princ c))
(defun princ4 (a b c d) (princ a)(princ b)(princ c)(princ d))
;==========================================================
;======================================
; box with buttom center point ip
(defun box_bc (ip L W H /   )
  ;(command "ucs" "O" ip)
  (command "ucs" "O" (rpoint3d ip 0 0 (/ H 2.0)))
  (command "box" "CE" "0,0,0" "L" L W H)
  (command "ucs" "P")(princ)
)
;======================================