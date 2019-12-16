(load "utils")

(defun make-vector (x y) (cons x y))

(defun x (vector) (car vector))

(defun y (vector) (cdr vector))

(defun add (vector1 vector2)
  (make-vector (+ (x vector1) (x vector2))
               (+ (y vector1) (y vector2))))

(defun sub (vector1 vector2)
  (make-vector (- (x vector1) (x vector2))
               (- (y vector1) (y vector2))))

(defun scale (vector n)
  (make-vector (* (x vector) n) (* (y vector) n)))

(defun cross-product (vector1 vector2)
  (- (* (x vector1) (y vector2))
     (* (y vector1) (x vector2))))

(defun make-segment (start direction)
  (list :start start :direction direction))

(defun start (segment)
  (getf segment :start))

(defun direction (segment)
  (getf segment :direction))

(defun len (segment)
  (let ((x (x (direction segment)))
        (y (y (direction segment))))
    (floor (sqrt (+ (* x x) (* y y))))))

;; Using approach outlined here:
;; https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
(defun find-intersection (segment1 segment2)
  (let* ((s1 (start segment1))
         (s2 (start segment2))
         (d1 (direction segment1))
         (d2 (direction segment2))
         (d1xd2 (cross-product d1 d2))
         (s2-s1 (sub s2 s1)))
    ;; Didn't bother with handling the case of overlapping segments
    (if (zerop d1xd2)
        nil
        (let ((pos1 (/ (cross-product s2-s1 d2) d1xd2))
              (pos2 (/ (cross-product s2-s1 d1) d1xd2)))
          (cons (* pos1 (len segment1)) (* pos2 (len segment2)))))))

(defun parse-segment (segment)
  (let* ((direction (subseq segment 0 1))
         (distance (parse-integer (subseq segment 1))))
    (cond ((string= "R" direction) (make-point distance 0))
          ((string= "L" direction) (make-point (- distance) 0))
          ((string= "U" direction) (make-point 0 distance))
          ((string= "D" direction) (make-point 0 (- distance))))))

(defun manhattan-distance (point)
  (+ (abs (x point)) (abs (y point))))

(defun path->lines (path)
  (loop
     for segment-desc in path
     for start = (make-vector 0 0) then (add start direction)
     for direction = (parse-segment segment-desc)
     collect (make-segment start direction)))

(defun load-data ()
  (destructuring-bind (line1 line2) (file->lines "advent-3.txt")
    (let ((wire1 (path->lines (csv->list line1)))
          (wire2 (path->lines (csv->list line2))))
    (list :wire1 wire1 :wire2 wire2))))

(defun find-all-intersections (wire1 wire2)
  (loop named outer
     for l1 in wire1
     append (loop
               for l2 in wire2
               for point = (find-intersection l1 l2)
               when point
               do (print point)
               collect point)))

(defun solve ()
  (plist-bind (wire1 wire2) (load-data)
    (let* ((intersections (find-all-intersections wire1 wire2)))
      nil)))
;;          (distances (mapcar #'manhattan-distance intersections)))
