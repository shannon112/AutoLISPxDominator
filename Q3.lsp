;================================================
;   Q2.LSP --- 
;================================================
(defun c:Q3( / ip nn n dlist r1 r2 r3 r4 r5 H H1 H2)
(if (null c:trod)(load "trod.lsp"))
(if (null tan)(load "util.lsp"))
  (graphscr)(command "osnap" "none")
   (if (and (setq ip (getpoint "\nInput the insertion point <exit>: "))
            (setq n (getreal "\nInput the n <exit>: "))
            (setq dlist (finddat "Q3.dat" n))
       );end-if-test
       (progn
         (setq r1 (nth 2 dlist)
	       r2 (nth 3 dlist)
	       r3 (nth 4 dlist)
	       r4 (nth 5 dlist)
               r5 (nth 6 dlist)
	       H (nth 7 dlist)
	       H1 (nth 8 dlist)
	       H2 (nth 9 dlist)
	       nn (last dlist)
         )
         (drawq2 ip nn n r1 r2 r3 r4 r5 H H1 H2)
	 (drawq22 ip nn n r1 r2 r3 r4 r5 H H1 H2)
       );end-if-then
   );end-if
);end-c:ew()
;================================================
(defun drawq2( ip n nn r1 r2 r3 r4 r5 H H1 H2 / theta1 theta2 th dth th1 th2 th3 th4 p1 p2 p3 p4 p5 p6 e1 s1s)
  (setq theta1 (/ (* 2 pi) (* n 8.0))
	theta2 (/ (* 1.5 pi) (* n 2.0))
        th 0.0    ;--- initial position
	dth (+ (+ theta1 theta2) (+ theta1 theta2))
  )
  (setq e1 (entlast))
  (repeat 5
    (setq th1 (- th (+ theta1 theta2))
          th2 (- th theta2)
          th3 (+ th theta2)
	  th4 (+ th (+ theta1 theta2))
	  p1 (polar ip th1 r3)
	  p2 (polar ip th2 r3)
          p3 (polar ip th2 r5)
          p4 (polar ip th3 r5)
          p5 (polar ip th3 r3)
          p6 (polar ip th4 r3)
          th (+ th dth)
    )
    (command "arc" "c" ip p1 p2)
    (command "line" p2 p3 "")
    (command "arc" "c" ip p3 p4)
    (command "line" p4 p5 "")
    (command "arc" "c" ip p5 p6)
  );end-repeat
  ;(command "circle" ip r1)
  (setq s1 (sset e1 (entlast)))
  (command "region" s1 "")
  (command "extrude" (entlast) "" H1 "")
  ;(redraw)
  (princ)
);end-ew()
;================================================
;================================================
(defun drawq22( ip n nn r1 r2 r3 r4 r5 H H1 H2 / theta1 theta2 th dth e2 e3 e4 s2 s3)
  (setq theta1 (/ (* 2 pi) (* n 8.0))
	theta2 (/ (* 1.5 pi) (* n 2.0))
        th 0.0    ;--- initial position
	dth (+ (+ theta1 theta2) (+ theta1 theta2))
  )
  (command "circle" ip r5)
  (command "region" (entlast) "")
  (setq e3 (entlast))
  (command "circle" ip r1)
  (command "region" (entlast) "")
  (setq e4 (entlast))
  (repeat 5
    (setq p1 (polar ip th r4)
          th (+ th dth)
    )
    (command "circle" p1 3)
    (command "region" (entlast) "")
  );end-repeat  
  (setq s3 (sset e4 (entlast)))
  (command "subtract" e3 "" e4 s3 "")
  (command "extrude" (entlast) "" (+ H1 H2) "")
  ;(redraw)
  (princ)
);end-ew()
;===============================================