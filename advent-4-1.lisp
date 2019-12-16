(defun same-digits? (num)
  (loop for n = (multiple-value-bind (q r) (floor num 10) q)

(defun matches? (num)
  (and
   (same-digits? num)
   (never-decrease? num)))

(defun solve ()
  (loop
     for num from 347312 to 805915
     counting (matches? num)))
