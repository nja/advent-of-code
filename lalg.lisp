;;;; lalg.lisp

(in-package #:lalg)

(defun determinant (rows &optional skip-cols)
  (let ((result 0)
        (sign -1))
    (dotimes (col (length (car rows)) result)
      (unless (member col skip-cols)
        (if (null (cdr rows))
            (return-from determinant (elt (car rows) col))
            (incf result (* (setq sign (- sign))
                            (elt (car rows) col)
                            (determinant (cdr rows) (cons col skip-cols)))) )))))

(defun array-determinant (matrix)
  (labels
      ((det (row skip-cols)
         (let ((result 0)
               (sign -1))
           (dotimes (col (array-dimension matrix 1) result)
             (unless (member col skip-cols)
               (if (eql row (1- (array-dimension matrix 0)))
                   (return-from det (aref matrix row col))
                   (incf result (* (setf sign (- sign))
                                   (aref matrix row col)
                                   (det (1+ row) (cons col skip-cols))))))))))
    (det 0 nil)))

(defun replace-column (rows column i)
  (when rows
    (cons (loop for x in (car rows)
                for col from 0
                collect (if (eql col i)
                            (car column)
                            x))
          (replace-column (cdr rows) (cdr column) i))))

(defun cramers (coefficients constant-terms)
  (let ((deta (determinant coefficients)))
    (loop for i from 0 below (length constant-terms)
          for ai = (replace-column coefficients constant-terms i)
          collect (/ (determinant ai) deta))))
