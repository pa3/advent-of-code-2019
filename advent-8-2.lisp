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

(defun layers->digits-layers (layers)
  (loop
     for i from 0 to (1- (* +width+ +height+))
     collect (reverse (loop
                         for layer in layers
                         collect (nth i layer)))))

(defun solve ()
  (let* ((digits (layers->digits-layers (digits->layers (load-data))))
         (merged (loop for digit in digits
                    collect (loop
                               for d in digit
                               with res = 2
                               do (setf res (if (= 2 d) res d))
                               finally (return res))))
         (merged-readable (mapcar #'(lambda (x) (if (= x 0) "." "x")) merged)))
    (loop
       for i from 0 to (1- +height+)
       do (format t "~{~a~}~%" (subseq merged-readable (* i +width+) (* (1+ i) +width+))))))
