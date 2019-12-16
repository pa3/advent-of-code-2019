(ql:quickload "cl-utilities")

(defun file->lines (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
       while line
       collect line)))

(defun csv->list (csv)
  (cl-utilities:split-sequence #\Comma csv))

(defun csv->ints (csv)
  (mapcar #'parse-integer
          (csv->list csv)))

(defmacro plist-bind (args exp &body body)
  `(destructuring-bind
         (&key ,@args &allow-other-keys)
       ,exp
     ,@body))
