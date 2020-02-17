(load "utils.lisp")

(defvar *objects* nil)

(defclass object ()
  ((name :initarg :name :reader name)
   (orbit :initform nil :accessor orbit)
   (satellites :initform nil :accessor satellites)))

(defun make-object (name)
  (make-instance 'object :name name))

(defun add-satellite (object satellite)
  (push satellite (satellites object))
  (setf (orbit satellite) object))

(defun name->obj (name)
  (let ((existing (gethash name *objects*)))
    (if existing
        existing
        (let ((obj (make-object name)))
          (setf (gethash name *objects*) obj)
          obj))))

(defun apply-config-line (line)
  (let* ((names (cl-utilities:split-sequence #\) line))
         (object (name->obj (first names)))
         (sattelite (name->obj (second names))))
    (add-satellite object sattelite)))

(defun init ()
  (setf *objects* (make-hash-table :test #'equal))
  (let ((lines (file->lines "advent-6.txt")))
    (loop for line in lines do (apply-config-line line))))

(defun path-to (object)
  (loop
     for orbit = (orbit object) then (orbit orbit)
     while (not (null orbit))
     collecting orbit into path
     finally (return (nreverse path))))

(defun common-prefix (list1 list2)
  (loop
     for i in list1
     for j in list2
     while (equal i j)
     collecting i))

(defun solve ()
  (init)
  (let* ((path-to-you (path-to (name->obj "YOU")))
         (path-to-san (path-to (name->obj "SAN")))
         (common-prefix-len (length (common-prefix path-to-you path-to-san))))
    (+ (- (length path-to-san) common-prefix-len)
       (- (length path-to-you) common-prefix-len))))
