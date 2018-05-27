;==========================================================
;   BOLT3D.LSP --- Bolt, Solid Model
;
;   Programmer : Tien-Tung Chung, NTUME, Feb. 8, 2003
;   needs UTIL.LSP : tan(), rpoint3d(), ...
;   needs THREAD.DAT, HexBolt.DAT, HexNut.DAT, HexCapsc.DAT
;
;   Note(92/03/01):
;      Lisp calls to SOLPROF Can not be executed  more than
;          3 times in AutoCAD 2000 .
;      It is OK in AutoCAD 2002.
;
;   Usage:
;      Command: BOLTEX1   -> Example1
;
;==========================================================
 (if (null tan)(load "util.lsp"))
 (command "facetres" 5)(command "dispsilh" 1)
;==========================================================
(defun c:bb (/) (c:ihbex1))
(defun c:ihbex1 ( / ip d dlist p len )
(command "ucs" "W")
  (setq ip '(0 0 0)
        ;d 20  len 50
        d 4  len 10
        dlist (finddat "thread.dat" d)
        p (cadr dlist)
        dlist (finddat "HexCapSc.dat" d)
  )
  (ihb3d ip d p len dlist)  (princ)
)
;==========================================================
; Internal Hex cap screw bolt
(defun ihb3d (ip d p len dlist / ip2 e1)
  (command "ucs" "O" ip)
  (command "osnap" "none")
  (setq ip2 '(0 0 0))
  (ihb3d-head ip2 d p len dlist) (setq e1 (entlast))
  (ihb3d-body ip2 d p len dlist)
  (command "union" (entlast) e1 "")
  (command "ucs" "p")
  (princ)
)

(defun ihb3d-body (ip2 d p len dlist)
 (bolt3d-body ip2 d p len dlist)
)
;==========================================================
(defun ihb3d-head (ip d p len dlist / a30 a60 pi/2 H r d2 r2 b m e
                   r3 r4 ip2 p0 p1 p2 p3 p4 e1 e2)

  (setq a30 (dtr 30) a60 (dtr 60) pi/2 (/ pi 2.0)
        r (/ d 2.0)          H d
        d2 (nth 2 dlist)     e (nth 3 dlist)
        B (nth 4 dlist)      m (nth 5 dlist)
        r2 (/ d2 2.0)
        r3 (/ B 2.0)         r4 (/ r3 (cos a30))
        ip2 (rpoint3d ip 0 0 (- H m))
        ;p1 (polar p0 0.0 r4)    p2 (polar ip a60 r4)
  )
  ;--- draw the head cyclider
  (command "cylinder" ip r2 H)(setq e1 (entlast))
  ;--- draw the Hex socket
  (command "ucs" "O" ip2)
  (setq p0 '(0 0 0)
        p1 (polar p0 0.0 r4)    p2 (polar ip a60 r4)
  )
  (command "polygon" 6 "E" p1 p2)
  (command "extrude" (entlast) "" m "") (setq e2 (entlast))
  (command "ucs" "P")

  ;--- draw the head chamfer(a triangular ring)
  (command "ucs" "3p" ip (rpoint3d ip 0 0 -10) (rpoint3d ip 10 0 0) )
  (setq p0 '(0 0 0)
        p1 (polar p0 pi H)
        p2 (polar p1 pi/2 (- r2 e))
        p4 (polar p1 pi/2 r2)
        p3 (polar p4 0.0 e)
  )
  (command "pline" p2 p3 p4 "C")
  (command "revolve" (entlast) "" p0 p1 "")
  (command "subtract" e1 "" e2 (entlast) "")
  (command "ucs" "P")  (princ)
)
;==========================================================

(defun c:nutex1( / ip d dlist pitch )
  (command "ucs" "W")
  (setq ip '(0 0 0)    d 20
        dlist (finddat "thread.dat" d)
        pitch (cadr dlist)
        dlist (finddat "HexNut.dat" d)
  )
  (nut3d ip d pitch dlist)  (princ)
)
;==========================================================
(defun c:boltex1( / ip d len dlist pitch e1 e2)
  (command "ucs" "W")
  (setq ip '(0 0 0)  d 20  len 30
        dlist (finddat "thread.dat" d)
        pitch (cadr dlist)
        dlist (finddat "HexBolt.DAT" d)
  )
  (bolt3d ip d pitch len dlist)  (princ)
)
;==========================================================
;   Bolt and Nut, EX01
(defun c:BNEX1 (/ d ip ip2 L p L1 L2)
 (command "ucs" "W")
  (setq d 20  ip '(0 0 0)   L 70
        p (cadr (finddat "thread.dat" d))
        ip2 (rpoint3d ip 0 0 (- (* p 5) L))
        L1 (finddat "HexBolt.DAT" d)
        L2 (finddat "HexNut.dat" d)

  )
  (bolt3d ip d p L L1)
  (nut3d ip2 d p L2)
  (command "view" "seiso")
  (princ)
)
;==========================================================
(defun c:nut3d( / ip d dlist pitch )
 (graphscr) (command "osnap" "none")
 (command "ucs" "W")
 (if (and (setq ip (getpoint "\nInput insertion point <exit>: "))
          (setq d (getreal "\nInput diameter of the nut <exit>: "))
          (setq dlist (finddat "thread.dat" d))
          (setq pitch (cadr dlist))
          (setq dlist (finddat "HexNut.dat" d))
     )
     (progn
        (nut3d ip d pitch dlist)
     )
 );end-if
 (princ)
);end-c:nut3d()
;==========================================================
(defun c:bolt3d( / ip d len dlist pitch )
 (graphscr) (command "osnap" "none")
 (command "ucs" "W")
 (if (and (setq ip (getpoint "\nInput insertion point <exit>: "))
          (setq d (getreal "\nInput diameter of the bolt <exit>: "))
          (setq len (getdist ip "\nInput length of the bolt <exit>: "))
          (setq dlist (finddat "thread.dat" d))
          (setq pitch (cadr dlist))
          (setq dlist (finddat "HexBolt.DAT" d))
     )
     (progn
        (bolt3d ip d pitch len dlist)
     )
 );end-if
 (command "vpoint" "1,-1,1")
 (command "zoom" "E" "zoom" "0.8x")(princ)
);end-c:bolt3d()
;==========================================================
(defun c:xx( / ip d len dlist pitch )
 (graphscr) (command "osnap" "none")
 (command "ucs" "W")
 (if (and (setq ip '(0 0 0))
          (setq d 10)
          (setq len 15)
          (setq dlist (finddat "thread.dat" d))
          (setq pitch (cadr dlist))
          (setq dlist (finddat "HexBolt.DAT" d))
     )
     (progn
        (bolt3d ip d pitch len dlist)
     )
 );end-if
 (command "vpoint" "1,-1,1")
 (command "zoom" "E" "zoom" "0.8x")(princ)
);end-c:bolt3d()
;==========================================================
;
(defun bolt3d (ip d pitch len dlist / e1 ip2 dlist)
  (command "ucs" "O" ip)
  (command "osnap" "none")
  (setq ip2 '(0 0 0))
  (bolt3d-head ip2 d pitch len dlist) (setq e1 (entlast))
  (bolt3d-body ip2 d pitch len dlist)
  (command "union" (entlast) e1 "")
  (command "ucs" "p")
  (princ)
)
;==========================================================
(defun bolt3d-head (ip d pitch len dlist / a30 a60 pi/2 H B r2 d3 r3 r4 c e1
                     p0 p1 p2 p3 p4 )
  (setq a30 (dtr 30) a60 (dtr 60) pi/2 (/ pi 2.0)
        H (nth 1 dlist)  B (nth 2 dlist)
        r2 (/ B 2.0) 
        d3 (nth 3 dlist)   r3 (/ d3 2.0)
        r4 (/ r2 (cos a30))
        p1 (polar ip 0.0 r4)    p2 (polar ip a60 r4)
  )
  (command "polygon" 6 "E" p1 p2)
  (command "extrude" (entlast) "" H "") (setq e1 (entlast))

  (command "ucs" "3p" ip (rpoint3d ip 0 0 -10) (rpoint3d ip 10 0 0) )
  (setq p0 '( 0 0 0)
        c (* (- r4 r3) (tan a30))
        p1 (polar p0 pi H)
        p2 (polar p1 pi/2 r3)
        p4 (polar p1 pi/2 r4)
        p3 (polar p4 0.0 c)
  )
  (command "pline" p2 p3 p4 "C")
  (command "revolve" (entlast) "" p0 p1 "")
  (command "subtract" e1 "" (entlast) "")
  (command "ucs" "P")  (princ)
)
;==========================================================
;  draw the thread part of the bolt
(defun bolt3d-body (ip d pitch len dlist / r ri a30 a60 a135 a120 k L
                       ps pe p0 p1 p1a p1b p2 pi/2)
  (setq p1 (rpoint3d ip 0 0 -10)
        p2 (rpoint3d ip 10 0 0)
  )
  (command "ucs" "3p" ip p1 p2)
  (setq r (/ d 2.0) pi/2 (/ pi 2.0)
        a30 (dtr 30) a60 (dtr 60) a135 (dtr 135) a120 (dtr 120)
        k (* pitch (cos a30))     ri (- r k)
        ps '(0 0 0)
        pe (polar ps 0.0 len)
        p0 (polar pe pi/2 ri)
        p1a (polar p0 a135 pitch)
        p2 (polar p0 pi pitch)
        p1b (polar p2 a60 pitch)
        p1 (inters p0 p1a p2 p1b)
        L (- len pitch)
  )
  (command "pline" pe p0 p1 p2)
  (while (> L pitch)
    (setq p1 (polar p2 a120 pitch)
          p2 (polar p2 pi pitch)
          L (- L pitch)
    )
    (command p1 p2)
  );end-while
  (command (polar ps pi/2 ri) ps "C")  ;end pline
  (command "revolve" (entlast) "" ps pe "")
  (command "ucs" "P")  (princ)
);end_bolt3d()
;==========================================================
(defun nut3d (ip d pitch dlist / e1 ip2)
  (command "ucs" "O" ip)
  (setq ip2 '(0 0 0))
  (nut3d-body ip2 d pitch dlist) (setq e1 (entlast))
  (nut3d-thread ip2 d pitch dlist)
  (command "subtract" e1 "" (entlast) "") (princ)
  (command "ucs" "p")
)
;==========================================================
(defun nut3d-body (ip d pitch dlist / a30 a60 H r r2 B c e1 e2 e3
                    r3 d4 r4 pi/2 p0 p1 p2 p3 p4 )

  (setq a30 (dtr 30) a60 (dtr 60) pi/2 (/ pi 2.0)
        H (nth 1 dlist)  ;H (* d (/ 2.0 3.0))
        B (nth 3 dlist)  d4 (nth 4 dlist)
        r (/ d 2.0)
        r2 (/ B 2.0)  ;r2 (* r 1.5)
        r3 (/ r2 (cos a30))
        r4 (/ d4 2.0)
        p1 (polar ip 0.0 r3)    p2 (polar ip a60 r3)
  )
  (command "polygon" 6 "E" p1 p2)
  (command "extrude" (entlast) "" H "") (setq e1 (entlast))

  (command "ucs" "3p" ip (rpoint3d ip 0 0 -10) (rpoint3d ip 10 0 0) )
  ; draw top ring
  (setq p0 '(0 0 0)
        ;r4 (* r3 0.98)   % 0.99 of r2
        c (* (- r3 r4) (tan a30))
        p1 (polar p0 pi H)
        p2 (polar p1 pi/2 r4)
        p4 (polar p1 pi/2 r3)
        p3 (polar p4 0.0 c)
  )
  (command "pline" p2 p3 p4 "C")
  (command "revolve" (entlast) "" p0 p1 "") (setq e2 (entlast))
  ; draw bottom ring
  (setq p0 '( 0 0 0)
        p1 (polar p0 pi H)
        p2 (polar p0 pi/2 r4)
        p4 (polar p0 pi/2 r3)
        p3 (polar p4 pi c)
  )
  (command "pline" p2 p3 p4 "C")
  (command "revolve" (entlast) "" p0 p1 "") (setq e3 (entlast))

  (command "subtract" e1 "" e2 e3 "")

  (command "ucs" "P")  (princ)
)
;==========================================================
(defun nut3d-thread (ip d pitch dlist / r r1 a30 k L H p/2
                       ps pe p0 p1 p2 p3 p4 p5)
  (setq p1 (rpoint3d ip 0 0 -10)
        p2 (rpoint3d ip 10 0 0)
  )
  (command "ucs" "3p" ip p1 p2)

  (setq r (/ d 2.0)
        H (nth 1 dlist)  ;H (* d (/ 2.0 3.0))
        a30 (dtr 30)       k (* pitch (cos a30))
        r1 (- r k)         L H
        p/2 (/ pitch 2.0)
        pe '(0 0 0)
        ;ps (polar pe pi (+ L (* pitch 1.0)))
        ps (polar pe pi H)
        p1 (polar pe pi/2 r)
        p2 (rpoint3d pe (- p/2) r1 0)
        p3 (polar p1 pi pitch)
        L (- L pitch)
  )
  (command "pline" pe p1 p2 p3)

  (while (> L pitch)
    (setq p2 (polar p2 pi pitch)
          p3 (polar p3 pi pitch)
          L (- L pitch)
    )
    (command p2 p3)
  );end-while

  ;--- draw the last thread
  (if (> L 0.0)
     (progn
       (setq p1 p3
             p2 (polar p2 pi pitch)
             p3 (polar ps pi/2 r)
             p4 (rpoint ps p/2 r1)
             p5 (inters p1 p2 p3 p4)
       )
       (command p5 p3)
     )
  );end-if
  ;(setq p3 (rpoint p3 0 (- r)))
  ;(princ "\np1,p2,p3,p4,p5,ps = ")
  ;(princ p1)(princ p2)(princ p3)(princ p4)(princ p5)(princ ps)
  (command ps "C")  ;end pline
  (command "revolve" (entlast) "" ps pe "")

  (command "ucs" "P")  (princ)
);end_bolt3d()
;==========================================================
;--- Solid Profile, Top view
(defun c:spt (/ s1 s2)
  (princ "\nSelect solids : ")  (setq s1 (ssget))
; (command "tilemode" 0) ;(command "mspace")
  (command "layout" "set" "layout1")
  (command) (command "mspace")
  (command "view" "Top")  (command "zoom" "E")
; (command "vpoint" "0,0,1")
  (command "solprof" s1 "" "" "" "")
  (setq s2 (ssget "X" (list (cons 8 "PV-*")))  )
  (command "chprop" s2 "" "LA" (getvar "clayer") "" )
  (command "cutclip" s2 "")
  (command "model")
  ;(command "pasteclip" pause)
  (eraselayer "PH-*")  (eraselayer "PV-*")
  (setq s1 nil s2 nil)  (gc)  (princ)
)
;==========================================================
;--- Solid Profile, Right view
(defun c:spr (/ s1 s2 ln)
  (princ "\nSelect solids : ")  (setq s1 (ssget))
  (command "layout" "set" "layout1")
      ;(command "tilemode" 0)
  (command) (command "mspace")
  (command "view" "Right")  (command "zoom" "E")
     ;(command "ucs" "V")
  (command "solprof" s1 "" "" "" "")
  (command "model")
  (command "view" "Right")  (command "UCS" "V") ; needed for A2002

  (setq s2 (ssget "X" (list (cons 8 "PV-*")))
        ln (getvar "clayer")
  )
  (command "chprop" s2 "" "LA" ln "" )
  (command "cutclip" s2 "")
  (command "UCS" "P")

  ;(command "pasteclip" pause)
  (eraselayer "PH-*")  (eraselayer "PV-*")
  (setq s1 nil s2 nil)  (gc)  (princ)
)
;==========================================================
;--- Solid Profile, Front view
(defun c:spf (/ s1 s2 ln)
  (princ "\nSelect solids : ")  (setq s1 (ssget))
  (command "layout" "set" "layout1")
      ;(command "tilemode" 0)
  (command) (command "mspace")
  (command "view" "Front")  (command "zoom" "E")
     ;(command "ucs" "V")
  (command "solprof" s1 "" "" "" "")
  (command "model")
  (command "view" "Front")  (command "UCS" "V") ; needed for A2002

  (setq s2 (ssget "X" (list (cons 8 "PV-*")))
        ln (getvar "clayer")
  )
  (command "chprop" s2 "" "LA" ln "" )
  (command "cutclip" s2 "")
  (command "UCS" "P")

  ;(command "pasteclip" pause)
  (eraselayer "PH-*")  (eraselayer "PV-*")
  (setq s1 nil s2 nil)  (gc)  (princ)
)
;==========================================================
;--- Solid Profile, South-East view
(defun c:spse (/ s1 s2 ln)
  (princ "\nSelect solids : ")  (setq s1 (ssget))
  (command "layout" "set" "layout1")
  (command) (command "mspace")
  (command "view" "seiso")  (command "zoom" "E")

  (command "solprof" s1 "" "" "" "")
  (command "model")
  (command "view" "seiso")  (command "UCS" "V") ; needed for A2002
  (setq s2 (ssget "X" (list (cons 8 "PV-*")))
        ln (getvar "clayer")
  )
  (command "chprop" s2 "" "LA" ln "" )
  (command "cutclip" s2 "")
  (command "ucs" "P")
  ;(command "pasteclip" pause)
  (eraselayer "PH-*")  (eraselayer "PV-*")
  (setq s1 nil s2 nil)  (gc)  (princ)
)
;==========================================================
(defun eraselayer (LN / ss)
  (setq ss (ssget "X" (list (cons 8 LN)))  )
  (if ss (command "erase" ss "") )  (princ)
)
;==========================================================