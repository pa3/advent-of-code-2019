(load "utils.lisp")

(defvar *objects* (make-hash-table :test #'equal))

(defun name (object)
  (car object))

(defun sattelites (object)
  (cdr object))

(defun (setf sattelites) (new-sattelites object)
  (rplacd object new-sattelites))

(defun add-sattelite (object sattelite)
  (push sattelite (sattelites object)))

(defun name->obj (name)
  (let ((existing (gethash name *objects*)))
    (if existing
        existing
        (let ((obj (cons name nil)))
          (setf (gethash name *objects*) obj)
          obj))))

(defun apply-config-line (line)
  (let* ((names (cl-utilities:split-sequence #\) line))
         (object (name->obj (first names)))
         (sattelite (name->obj (second names))))
    (add-sattelite object sattelite)))

(defun count-orbits (object &optional (depth 0))
  (+ depth (loop
              for obj in (sattelites object)
              summing (count-orbits obj (1+ depth)))))

(defun solve ()
  (setf *objects* (make-hash-table :test #'equal))
  (let ((lines (file->lines "advent-6.txt")))
    (loop for line in lines do (apply-config-line line))
    (count-orbits (name->obj "COM"))))
