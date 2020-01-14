(defun nth-digit (num n)
  (multiple-value-bind (q r)
      (floor (floor num (expt 10 n)) 10)
    r))

(defun same-digits? (num)
  (loop
     with seq-length = 1
     for n from 0 to 4
     do (progn
          (unless (= (nth-digit num n)
                     (nth-digit num (1+ n)))
            (if (= seq-length 2) (return t)))
          (setf seq-length 1))
     finally (if (= seq-length 2) (return t))))

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
