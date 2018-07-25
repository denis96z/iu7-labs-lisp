(defun can-transpose (m)
  (if (listp m) (equal (list-length m) (all-items-equal (matrix-col-lengths m))) nil))

(defun transpose (m)
  (if (null (car m)) nil (cons (first-items m) (transpose (without-first-items m)))))

(defun first-items (m)
  (mapcar #'car m))

(defun without-first-items (m)
  (mapcar #'cdr m))

(defun matrix-col-lengths (m)
  (if (listp m) (mapcar #'list-length-or-nil m) nil))
  
(defun list-length-or-nil (lst)
  (if (listp lst) (list-length lst) nil))

(defun all-items-equal (lst)
  (if (null lst) nil (all-items-equal-to (car lst) (cdr lst))))

(defun all-items-equal-to (x lst)
  (if (null lst) x (and (equal x (car lst)) (all-items-equal-to x (cdr lst)))))
