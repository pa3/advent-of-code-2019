(load "utils.lisp")

(defun mass->fuel (mass) (- (floor (/ mass 3)) 2))
