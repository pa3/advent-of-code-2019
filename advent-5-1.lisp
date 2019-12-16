(load "utils.lisp")

(defvar *memory*)
(defvar *pointer*)
(defvar *on-halt*)

(defun read-next-int ()
  (let ((int (elt *memory* *pointer*)))
    (incf *pointer*)
    int))

(defclass parameter ()
  ((code :initarg :code)))
(defclass position-parameter (parameter) ())
(defclass immediate-parameter (parameter) ())

(defun make-parameter (mode code)
  (let ((class (if (= mode 0)
                   'position-parameter
                   'immediate-parameter)))
    (make-instance class :code code)))

(defgeneric value (parameter)
  (:documentation "Get parameter's value"))
(defmethod value ((parameter position-parameter))
  (let ((addr (slot-value parameter 'code)))
    (elt *memory* addr)))
(defmethod value ((parameter immediate-parameter))
  (slot-value parameter 'code))

(defgeneric (setf value) (new-value parameter)
  (:documentation "Set parameter's value"))
(defmethod (setf value) (new-value (parameter position-parameter))
  (let ((addr (slot-value parameter 'code)))
    (setf (elt *memory* addr) new-value)))
(defmethod (setf value) (new-value (eparameter immediate-parameter))
  (error "Operation is not permited"))

(defclass instruction ()
  ((params :accessor params)
   (param-modes :initarg :param-modes)))
(defclass add (instruction) ())
(defclass mul (instruction) ())
(defclass in (instruction) ())
(defclass out (instruction) ())
(defclass halt (instruction) ())

(defun nth-param (instruction n)
  (value (elt (params instruction) n)))
(defun (setf nth-param) (value instruction n)
  (setf (value (elt (params instruction) n)) value))

(defun read-next-param (mode)
  (let ((code (read-next-int)))
    (make-parameter mode code)))

(defun read-n-params (instruction n)
  (with-slots (param-modes) instruction
    (let ((params (loop
                     repeat n
                     for modes = param-modes then (floor modes 10)
                     for mode = (rem modes 10)
                     collect (read-next-param mode))))
      (setf (params instruction) params))))

(defgeneric read-params (instruction)
  (:documentation "Reads instrucion's params from memory"))
(defmethod read-params ((instrucion add)) (read-n-params instrucion 3))
(defmethod read-params ((instrucion mul)) (read-n-params instrucion 3))
(defmethod read-params ((instrucion halt)) (read-n-params instrucion 0))
(defmethod read-params ((instrucion in)) (read-n-params instrucion 1))
(defmethod read-params ((instrucion out)) (read-n-params instrucion 1))

(defgeneric exec (instruction)
  (:documentation "Executes instruction"))

(defmethod exec ((instruction add))
  (setf (nth-param instruction 2)
        (+ (nth-param instruction 0)
           (nth-param instruction 1))))

(defmethod exec ((instruction mul))
  (setf (nth-param instruction 2)
        (* (nth-param instruction 0)
           (nth-param instruction 1))))

(defmethod exec ((instruction halt))
  (funcall *on-halt*))

(defmethod exec ((instruction in))
  (format *query-io* ">")
  (force-output *query-io*)
  (let ((input (parse-integer (read-line *query-io*))))
    (setf (nth-param instruction 0) input)))

(defmethod exec ((instruction out))
  (format *query-io* "~a~%" (nth-param instruction 0)))

(defun make-instruction (opcode modes)
  (let ((class (cond ((= opcode 99) 'halt)
                     ((= opcode 1) 'add)
                     ((= opcode 2) 'mul)
                     ((= opcode 3) 'in)
                     ((= opcode 4) 'out)
                     (t (error "Unknown OPCODE")))))
    (make-instance class :param-modes modes)))

(defun read-next-instruction ()
  (multiple-value-bind (modes opcode) (floor (read-next-int) 100)
    (let ((instruction (make-instruction opcode modes)))
      (read-params instruction)
      instruction)))

(defun execute-next-instruction ()
  (let ((instruction (read-next-instruction)))
    (exec instruction)))

(defun execute-program (image)
  (let ((*memory* (copy-seq image))
        (*pointer* 0))
    (block execution
      (let ((*on-halt* #'(lambda () (return-from execution))))
        (loop do (execute-next-instruction))))
    *memory*))

(defun patch (program pos value)
  (setf (aref program pos) value))

(defun load-input ()
  (let* ((lines (file->lines "advent-5.txt"))
         (ints (csv->ints (first lines))))
    (make-array (length ints) :initial-contents ints)))

(defun load-input-1 ()
  (make-array 5 :initial-contents '(1002 4 3 4 33)))

(defun solve ()
  (let ((program-image (load-input)))
    (execute-program program-image)))
