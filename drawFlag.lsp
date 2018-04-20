
(defun c:flag (/ ip r1 r2 nt)
  (graphscr)(command "osnap" "none")
  (setq ip (getpoint "\Input the insertion point : ")
        r1 (getdist ip "\nInput the inner radius : ")
        r2 (getdist ip "\nInput the outer radius : ")
        nt (getint "\nInput no. of teeth <12>: ")
        nt (if nt nt 12)
  )
  (drawflag ip r1 r2 nt)
  (princ)
)

(defun drawflag (ip r1 r2 nt / p0 p1 p2 p3 dth dth/2
                               th1 th2 th3)
  (setq dth (/ pi nt 0.5) dth/2 (/ dth 2.0)
        th1 (- dth/2) th2 0.0 th3 dth/2
        p1 (polar ip th1 r1) p2 (polar ip th2 r2)
        p3 (polar ip th3 r1)
  )
  (command "circle" ip r1) ;(command "circle" ip r2)
  (command "pline" p1)
  (repeat nt
    (command p2 p3)
    (setq th2 (+ th2 dth) th1 (- th2 dth/2) th3 (+ th2 dth/2)
          p1 p3 p2 (polar ip th2 r2) p3 (polar ip th3 r1)
    )
  )
  (command "")
  (princ)
)

