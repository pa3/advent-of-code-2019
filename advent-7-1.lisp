(load "utils.lisp")

(defvar *memory*)
(defvar *pointer*)
(defvar *on-halt*)
(defvar *in* (lambda ()
               (format *query-io* ">")
               (force-output *query-io*)
               (parse-integer (read-line *query-io*))))
(defvar *out* (lambda (value) (format *query-io* "~a~%" value)))

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
(defclass jump-if-true (instruction) ())
(defclass jump-if-false (instruction) ())
(defclass less-than (instruction) ())
(defclass equals (instruction) ())
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
(defmethod read-params ((instrucion jump-if-true)) (read-n-params instrucion 2))
(defmethod read-params ((instrucion jump-if-false)) (read-n-params instrucion 2))
(defmethod read-params ((instrucion less-than)) (read-n-params instrucion 3))
(defmethod read-params ((instrucion equals)) (read-n-params instrucion 3))

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
  (let ((input (funcall *in*)))
    (setf (nth-param instruction 0) input)))

(defmethod exec ((instruction out))
  (funcall *out* (nth-param instruction 0)))

(defmethod exec ((instruction jump-if-true))
  (if (not (= (nth-param instruction 0) 0))
      (setf *pointer* (nth-param instruction 1))))

(defmethod exec ((instruction jump-if-false))
  (if (= (nth-param instruction 0) 0)
      (setf *pointer* (nth-param instruction 1))))

(defmethod exec ((instruction less-than))
  (if (< (nth-param instruction 0) (nth-param instruction 1))
      (setf (nth-param instruction 2) 1)
      (setf (nth-param instruction 2) 0)))

(defmethod exec ((instruction equals))
  (if (= (nth-param instruction 0) (nth-param instruction 1))
      (setf (nth-param instruction 2) 1)
      (setf (nth-param instruction 2) 0)))

(defun make-instruction (opcode modes)
  (let ((class (cond ((= opcode 99) 'halt)
                     ((= opcode 1) 'add)
                     ((= opcode 2) 'mul)
                     ((= opcode 3) 'in)
                     ((= opcode 4) 'out)
                     ((= opcode 5) 'jump-if-true)
                     ((= opcode 6) 'jump-if-false)
                     ((= opcode 7) 'less-than)
                     ((= opcode 8) 'equals)
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

(defun execute-program (image &key (in *in*) (out *out*))
  (let ((*memory* (copy-seq image))
        (*pointer* 0)
        (*in* in)
        (*out* out))
    (block execution
      (let ((*on-halt* #'(lambda () (return-from execution))))
        (loop do (execute-next-instruction))))
    *memory*))

(defun patch (program pos value)
  (setf (aref program pos) value))

(defun next-permutation! (sequence)
  "Returns next lexicographic ordered permutation of a given sequence. Will mutate provided sequence."
  (let* ((len (length sequence))
         (j (loop
               for j from (- len 2) downto 0
               for a[j] = (elt sequence j)
               for a[j+1] = (elt sequence (1+ j))
               do (if (< a[j] a[j+1]) (return j)))))
    (if j (let* ((a[j] (elt sequence j))
                 (l (loop
                       for l from (- len 1) downto (1+ j)
                       for a[l] = (elt sequence l)
                       do (if (< a[j] a[l]) (return l)))))
            (rotatef (elt sequence j) (elt sequence l))
            (concatenate 'list
                         (subseq sequence 0 (1+ j))
                         (reverse (subseq sequence (1+ j))))))))

(defun make-permutations (n)
  (let ((initial (loop for i from 0 to (1- n) collect i)))
    (loop for p = initial then (next-permutation! p)
       while p
       collect (copy-seq p))))

(defun load-input ()
  (let* ((lines (file->lines "advent-7.txt"))
         (ints (csv->ints (first lines))))
    (make-array (length ints) :initial-contents ints)))

(defun load-fake-input ()
  (let ((fake-input '(3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33
1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0)))
    (make-array (length fake-input) :initial-contents fake-input)))

(defun solve ()
  (let ((program-image (load-input))
        (phases-permutations (make-permutations 5)))
    (flet ((get-output-for (phase input)
             (let ((invocations 0)
                   result)
               (flet ((in ()
                        (incf invocations)
                        (if (= invocations 1)
                            phase
                            input))
                      (out (value) (setf result value)))
                 (execute-program program-image
                                  :in #'in
                                  :out #'out)
               result))))
      (loop
         for phases in phases-permutations
         maximizing (loop
                       for output = 0 then (get-output-for phase output)
                       for phase in phases
                       finally (return output))))))
