(load "utils")

(defun make-point (x y) (cons x y))

(defun x (point) (car point))

(defun y (point) (cdr point))

(defun add-points (point1 point2)
  (make-point (+ (x point1) (x point2))
              (+ (y point1) (y point2))))

(defun next-point (segment prev-point)
  (let* ((direction (subseq segment 0 1))
         (distance (parse-integer (subseq segment 1)))
         (delta (cond ((string= "R" direction) (make-point distance 0))
                      ((string= "L" direction) (make-point (- distance) 0))
                      ((string= "U" direction) (make-point 0 distance))
                      ((string= "D" direction) (make-point 0 (- distance))))))
    (add-points prev-point delta)))

(defun make-line (start end) (list start end))

(defun line-start (line) (car line))

(defun line-end (line) (cadr line))

(defun lines-intersection (line1 line2)
  (let ((x1 (x (line-start line1)))
        (x2 (x (line-end line1)))
        (x3 (x (line-start line2)))
        (x4 (x (line-end line2)))
        (y1 (y (line-start line1)))
        (y2 (y (line-end line1)))
        (y3 (y (line-start line2)))
        (y4 (y (line-end line2))))
    (cond ((and (< x1 x3 x2) (< y3 y1 y4)) (cons x3 y1))
          ((and (< y1 y3 y2) (< x3 x1 x4)) (cons x1 y3))
          (t nil))))

(defun manhattan-distance (point)
  (+ (abs (x point)) (abs (y point))))

(defun make-line (point1 point2)
  (let ((x1 (min (x point1) (x point2)))
        (x2 (max (x point1) (x point2)))
        (y1 (min (y point1) (y point2)))
        (y2 (max (y point1) (y point2))))
    (list (make-point x1 y1) (make-point x2 y2))))

(defun path->lines (path)
  (loop
     for segment in path
     for prev = (make-point 0 0) then point
     for point = (next-point segment prev)
     collect (make-line prev point)))

(defun load-data ()
  (destructuring-bind (line1 line2) (file->lines "advent-3.txt")
    (let ((wire1 (path->lines (csv->list line1)))
          (wire2 (path->lines (csv->list line2))))
    (list wire1 wire2))))

(defun find-all-intersections (wire1 wire2)
  (loop named outer
     for l1 in wire1
     append (loop
               for l2 in wire2
               for point = (lines-intersection l1 l2)
               when point
               collect point)))

(defun solve ()
  (destructuring-bind (wire1 wire2) (load-data)
   (let* ((intersections (find-all-intersections wire1 wire2))
          (distances (mapcar #'manhattan-distance intersections)))
      (loop for distance in distances minimize distance))))
