(defun c:cyc(/ ps pss r ni th0 theta deltad dist x0 y0 z0 pc)
  (graphscr)
  (if (and
        (setq ps (getpoint "\nstart point of the cycloid curve <exit>: "))
        (setq r (getdist ps "\nRadius of the generating circle <exit>: "))
        (setq ni (if (setq ni (getint "\nNo. of points <100>: ")) ni 100))
      );end-if-test
      (progn

	;draw line
	(setq pss ps)
	(command "line" ps)
	(setq ps (polar ps 0 (* 6.0 pi r)))
	(command ps)ss
	(command "")
	(setq ps pss)

	;draw circle
	(setq ps (polar ps ( / pi 2.0) r))
	(command "circle" ps r)
	(setq ps pss)	

	;draw cycloid curve
        (setq th0   (* 1.5 pi)
              deltad (/ (* 6.0 pi r) (float ni))
              dist 0.0             x0 (car ps)
              y0 (+ (cadr ps) r)   z0 (caddr ps)
        )
        (command "pline" ps)              ;start of the curve
        (repeat ni
          (setq dist (+ dist deltad)  theta (/ dist r)
                theta (- th0 theta)   x0 (+ x0 deltad)
                pc (list x0 y0 z0)       ;center point
                ps (polar pc theta r)
          )
          (command ps)
        );end-repeat
        (command "")                     ;end of the curve
      );end-if-then
  );end-if
  (redraw)
  (princ)       ;null echo
);end-cycloid()