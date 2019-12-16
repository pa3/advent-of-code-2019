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
(defun value (parameter)
  (let ((addr (slot-value parameter 'code)))
    (elt *memory* addr)))
(defun (setf value) (new-value parameter)
  (let ((addr (slot-value parameter 'code)))
    (setf (elt *memory* addr) new-value)))

(defclass instruction ()
  ((params :accessor params)))
(defclass add (instruction) ())
(defclass mul (instruction) ())
(defclass halt (instruction) ())

(defun nth-param (instruction n)
  (value (elt (params instruction) n)))
(defun (setf nth-param) (value instruction n)
  (setf (value (elt (params instruction) n)) value))

(defun read-next-param ()
  (let ((int (read-next-int)))
    (make-instance 'parameter :code int)))

(defun read-n-params (instruction n)
  (let ((params (loop
                   repeat n
                   collect (read-next-param))))
    (setf (params instruction) params)))

(defgeneric read-params (instruction)
  (:documentation "Reads instrucion's params from memory"))
(defmethod read-params ((instrucion add)) (read-n-params instrucion 3))
(defmethod read-params ((instrucion mul)) (read-n-params instrucion 3))
(defmethod read-params ((instrucion halt)) (read-n-params instrucion 0))

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
(defmethod exec ((instruction halt)) (funcall *on-halt*))

(defun make-instruction (opcode)
  (let ((class (cond ((= opcode 99) 'halt)
                     ((= opcode 1) 'add)
                     ((= opcode 2) 'mul)
                     (t (error "Unknown OPCODE")))))
    (make-instance class)))


(defun read-next-instruction ()
  (let* ((opcode (read-next-int))
         (instruction (make-instruction opcode)))
    (read-params instruction)
    instruction))

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
  (let* ((lines (file->lines "advent-2.txt"))
         (ints (csv->ints (first lines))))
    (make-array (length ints) :initial-contents ints)))

(defun solve ()
  (let ((program-image (load-input)))
    (loop
       named outer
       for noun from 0 to 99
       do (loop
             for verb from 0 to 99
             do (let ((image-copy (copy-seq program-image)))
                  (patch image-copy 1 noun)
                  (patch image-copy 2 verb)
                  (let ((final-memory-state (execute-program image-copy)))
                    (if (= 19690720 (elt final-memory-state 0))
                        (progn (format t "nount=~a verb=~a" noun verb)
                               (return-from outer)))))))))
