;================================================
;   EW.LSP --- External Washer
;
;   needs : EW.DAT, Ref. p11a-46 of HandBook
;
;   Programmer : Tien-Tung Chung, Jan. 26, 1997
;================================================
(if (null rotate) (load "util"));不load沒辦法轉
(defun c:ew( / ip s1 s2 s3 ip phi dlist d1 d2 n e1 e2)
   (graphscr)
   (setq s1 "\nInput the insertion point <exit>: "
         s2 "\nDia. = 3 4 5 6 8 10 12 16 20 24"
         s3 "\nInput the norminal diameter <exit>: "
   )
   (if (and (setq ip (getpoint s1))
            (princ s2)
            (setq phi (getreal s3))
            (setq dlist (finddat "ew.dat" phi))
       );end-if-test
       (progn
         (setq d1 (nth 1 dlist)
               d2 (nth 2 dlist)
               n (last dlist)
         )
         (setq e1 (entlast))
         (drawew ip d1 d2 n)
         (setq e2 (entlast))
         (rotate ip e1 e2 nil)
       );end-if-then
   );end-if
);end-c:ew()
;================================================
(defun drawew(ip d1 d2 n / d r r1 r2 c dth dth/2 dth/4 th
             th1 th2 th5 th6 p1 p2 p3 p4 p5)
  (setq d (/ (+ d1 d2) 2.0)
        r (/ d 2.0)
        r1 (/ d1 2.0)
        r2 (/ d2 2.0)
        c (- r2 r)
        dth (/ (* 2.0 pi) n)
        dth/2 (/ dth 2.0)
        dth/4 (/ dth 4.0)
        th 0.0    ;--- initial position
  )
  (repeat n
    (setq th1 (- th dth/2)
          th2 (- th dth/4)
          th5 (+ th dth/4)
          th6 (+ th dth/2)
          p1 (polar ip th1 r)
          p2 (polar ip th2 r)
          p3 (polar p2 th c)
          p5 (polar ip th5 r)
          p4 (polar p5 th c)
          p6 (polar ip th6 r)
          th (+ th dth)
    )
    (command "arc" "c" ip p1 p2)
    (command "line" p2 p3 "")
    (command "arc" "c" ip p3 p4)
    (command "line" p4 p5 "")
    (command "arc" "c" ip p5 p6)
  );end-repeat
  (command "circle" ip r1)
  ;(redraw)
  (princ)
);end-ew()
;================================================
