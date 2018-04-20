(defun c:s1(/ ip pe r nc len p th th1 th2 p1 p2 p3)
	(graphscr)
  	(if (and ;if  ; and can return true false
	  	(setq ip (getpoint "\nInsertion Point <exit>: "))
	  	(setq pe (getpoint "\nEnd point pf the spring <exit>: "))
	 	(setq r (getdist "\nRadius of the spring<exit>: "))
	      );end-and
  	    (progn
	          (setq nc (getint "\nNo. of coils<10>: "))
		  (setq nc (if nc nc 10))
		  (setq len (distance ip pe)
			th (angle ip pe)
			th1 (+ th (/ pi 2.0)) th2 (- th (/ pi 2.0))
			p (/ len nc)
			p1 (polar ip th2 r) p2 (polar ip th1 r)
  			)
  
  		  (command "pline" p2 p1)
  		  (setq p2 (polar p2 th (/ p 2.0))
			p3 (polar p1 th p)
  		  )	
  		  (repeat nc
	 		(command p2 p3)
   	 		(setq p2 (polar p2 th p)
	 	 	      p3 (polar p3 th p)
	  		)
	  	  );end-repeat
  		  (setq p2 (polar pe th1 r))
  		  (command p2 "")
  	    );end-then(progn)
	);end-if
(princ)
);end-of-spring