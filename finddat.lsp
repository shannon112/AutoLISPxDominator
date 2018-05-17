(defun finddat(fn di / fp dat d char)
  (setq fp (open fn "r"))
  (while (and (setq dat (read-line fp))
              ;--- test if a comment card
              (setq char (substr dat 1 1))
              (if (or (= char ";") (= dat ""))
                  t
                  (progn
                     (setq dat (read dat) d (car dat))
                     (/= d di)
                  )
              );end-if
         );end-while-test
  );end-while
  (close fp)
  (if (= d di)
      dat        ;if-then
      (progn     ;if-else
       (princ "\nData list with value - ")(princ di)
       (princ " not found in file - ") (princ fn)
       ;(pexit)
       nil
      )
  );end-if
);end-finddat()