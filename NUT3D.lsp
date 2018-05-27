;==========================================================
;   Nut3D.LSP --- Nut, Solid Model
;
;   needs UTIL.LSP : tan(), rpoint3d(), ...
;   needs THREAD.DAT, HexNut.DAT
;   Use SOLPROF.LSP to generate Solid Profiles of Solids
;
;   Programmer : Tien-Tung Chung, NTUME, Feb. 8, 2003
;==========================================================
 (if (null tan)(load "util.lsp"))
;==========================================================
;   Nut, Ex1
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
(defun nut3d2(ip d / pitch dlist )
 (graphscr) (command "osnap" "none")
 (setq dlist (finddat "thread.dat" d))
 (setq pitch (cadr dlist))
 (setq dlist (finddat "HexNut.dat" d))
 (nut3d ip d pitch dlist)
 (princ)
);end-c:nut3d()
;==========================================================
(defun nut3d (ip d pitch dlist / e1 ip2)
  (command "ucs" "O" ip)
  (setq ip2 '(0 0 0))
  (nut3d-body ip2 d pitch dlist) (setq e1 (entlast))
  (nut3d-thread ip2 d pitch dlist)
  (command "subtract" e1 "" (entlast) "") (princ)
  (command "ucs" "p")
  (command "zoom" "E" "" "0.8x")(princ)
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
        ;p1 (polar ip 0.0 r3)    p2 (polar ip a60 r3)
        p1 (polar ip (- a30) r3)   p2 (polar ip a30 r3)
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
        ;p1 (polar p0 pi H)
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
  (command ps "C")  ;end pline
  (command "revolve" (entlast) "" ps pe "")

  (command "ucs" "P")  (princ)
);end_nut3d-thread()
;==========================================================
