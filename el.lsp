;===================================================
;  EL.LSP --- Entity List ( List the entity datas )
;  Programmer : T.T. Chung, April, 27, 1994
(defun c:el( / e en ed)
  (initget "Yes No")
  (setq flag (getkword "\nYes/No <No>: "))
  (if (null flag) (setq flag "No"))
  (princ "\nflag = ")(princ flag)
  (if (= flag "Yes")
      (progn
        (setq fn (getstring "\nOutput file name <exit>: "))
        (if (/= "" fn)
            (setq fp (open fn "w"))
            (setq fp nil)
        )
      );end-if-then
      (if (/= flag "No")
          (princ "\nError option in c:el() !!")
      );end-if-else
  )
  (if (setq e (entsel "\nSelect an entity <exit>: "))
      (progn
        (setq en (car e)
              ed (entget en)
              et (cdr (assoc 0 ed))
        )
        (textscr)
        (if (= et "POLYLINE")
            (prtpline en fp) ;for PLINE
            (prted ed fp)  ;for other entity
        )
      );end-if-then
  );end-if
  (if fp (close fp))
  (princ)
);end-c:el()
;================================================
(defun prtpline (en fp / )
  (setq ed (entget en))
  (prted ed fp)
  (while (and (setq en (entnext en))
              (setq ed (entget en))
              (setq et (cdr (assoc 0 ed)))
              (/= "SEQEND" et)
         )
         (prted ed fp)
  )
  (prted ed fp) ;print SEQEND
)
;================================================
(defun prted(dl fp / a)
  (if (listp dl)
      (progn
        (if fp
          (progn
            (princ "\n(" fp)
            (foreach el dl (princ "\n " fp)(princ el fp))
            (princ "\n)" fp)
          )
          (progn
            (princ "\n(")
            (foreach el dl (princ "\n ")(princ el))
            (princ "\n)")
          )
        )
      );end-if-then
      (princ "\nError in prted() --- not a list !")
  );end-if
  (princ)
);end-prted()
;=======================================================
;   c:eldata --- Generate basic entities for obtaining
;                 DXF datas
(defun c:eldata ( / )
  (graphscr)
  (command "layer" "s" "0" "")

  (command "point" "75,100")
  (command "circle" "100,100" 25)
  (command "arc" "c" "150,100" "@50<30" "150,150")
  (command "layer" "s" "dim" "")
  (command "text" "200,100" 15.0 30 "abcd123")
  (command "layer" "s" "cen" "")
  (command "line" "200,50" "300,150" "")

  (command "layer" "s" "0" "")
  (command "pline" "50,250" "100,200" "150,250" "200,200" "")
  (command "3dpoly" "50,300" "100,250" "150,300" "200,250" "")
  (command "spline" "50,200" "100,150" "150,200" "200,150" "" "" "")
  (command "zoom" "E")(command "zoom" "0.8x") (princ)
);end-function
;=======================================================
