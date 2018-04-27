(defun c:invv( / pc ps rb pi/2 phi ni dth thi phii p0 len)
	(graphscr)(command "osnap" "none")
	(if (and (setq pc (getpoint "\ncenter point of the base circle <exit>: "))
		 (setq rb (getdist pc "\nradius of the base circle <exit>: "))
		);end of if-test
	  	(progn
		  (command "circle" pc rb)
		  (setq pi/2 (/ pi 2.0)
			ps (polar pc pi/2 rb)
			phi (getreal "\nGenerating angle(in pi's)<0.5pi>: ")
			phi (if phi phi 0.5)
			phi (* pi phi)
			ni (getint "\nNO. of intervals <30>: ")
			ni (if ni ni 30)
			dth (/ phi (float ni))
			phii 0.0
			)
		;start of drawing the involute curve
		  (command "pline" ps)
		  (repeat ni
			(setq phii (+ phii dth)
			      thi (- pi/2 phii)
			      p0 (polar pc thi rb)
			      len (* rb phii)
			      ps (polar p0 (+ thi pi/2) len)
			)
			(command ps)
		)
		  	(command "")
		);end of if-then
	);end if
	(princ)
);end fun