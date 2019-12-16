(load "utils")

(defconstant +width+ 25)
(defconstant +height+ 6)

(defun digits->layers (digits)
  (let* ((layer-size (* +width+ +height+))
         (n-layers (/ (length digits) layer-size)))
    (loop
       for i from 0 to (1- n-layers)
       collect (subseq digits (* i layer-size) (* (1+ i) layer-size)))))

(defun load-data ()
  (let* ((input (first (file->lines "advent-8.txt")))
         (digits (loop for d across input collect (parse-integer (string d)))))
    digits))

(defun solve ()
  (let ((layers (digits->layers (load-data))))
    (flet ((calc-sums (layer)
             (loop
                for d in layer
                counting (= 0 d) into zeros
                counting (= 1 d) into ones
                counting (= 2 d) into twos
                finally (return (list :zeros zeros :ones ones :twos twos)))))
      (let* ((sums (mapcar #'calc-sums layers))
             (sorted-sum (sort sums #'< :key (lambda (x) (getf x :zeros))))
             (with-fewest-zeros (first sorted-sum)))
        (* (getf with-fewest-zeros :ones) (getf with-fewest-zeros :twos))))))
