(defun nth-digit (num n)
  (multiple-value-bind (q r)
      (floor (floor num (expt 10 n)) 10)
    r))

(defun same-digits? (num)
  (loop for n from 0 to 4
     thereis (= (nth-digit num n)
                (nth-digit num (1+ n)))))

(defun never-decreases? (num)
  (loop for n from 0 to 4
     always (>= (nth-digit num n)
                (nth-digit num (1+ n)))))

(defun matches? (num)
  (and
   (same-digits? num)
   (never-decreases? num)))

(defun solve ()
  (loop
     for num from 347312 to 805915
     counting (matches? num)))
