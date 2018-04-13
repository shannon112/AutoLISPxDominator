; test.lsp
(defun c:tt ( / p1 p2)
	(setq p1 '(0 0) p2 '(10 10))
	(command "erase" "all" "")
	(command "line" p1 p2 "")
	(command "circle" p1 5)
	(command "circle" p2 3)
  )

(defun sum (n / i s)
	(setq i 1 s 0)
  	(repeat n
		(setq s (+ s i))
	  	(setq i (+ i 1))
	  )
  s;retrun value
  )

(defun c:tri ( / ip p2 p3 th1 th2 len)
	(graphscr)
	(command "osnap" "none")
	(setq ip (getpoint "\n Input first point"))
	(setq p2 (getpoint ip "\n Input second point"))

	(setq len (distance ip p2)
		th1(angle ip p2)
	      	th2(+ th1 (/ pi 3.0))
	      	p3 (polar ip th2 len)
	      )
	(command "line" ip p2 p3 "c")
	(princ)
)