;;;; aoc.lisp

(in-package #:aoc)

(defun system-pathname (name &key type)
  (asdf:system-relative-pathname :advent-of-code name :type type))

(defun input-path (year day)
  (system-pathname (format nil "~4,'0d/day~2,'0d.input.txt" year day)))

(defun input (&optional (day (default-day)) (year (default-year)))
  (unless (probe-file (input-path year day))
    (save-input year day))
  (let ((*package* (or (package-for year day) *package*)))
    (strip-cr (alexandria:read-file-into-string (input-path year day)))))

(defun package-for (year day)
  (find-package (format nil "AOC~4,'0d.DAY~2,'0d" year day)))

(defun answer (function)
  (destructuring-bind (year day) (scan-package-name (symbol-package function))
    (let ((*package* (symbol-package function)))
      (funcall function (input day year)))))

(defmacro defanswer (day &rest answers)
  (let* ((prefix (first (str:split #\. (package-name *package*))))
         (suffix (symbol-name day))
         (package (find-package (concatenate 'string prefix "." suffix))))
    `(fiasco:deftest ,(a:symbolicate suffix)
         ()
       ,@(mapcar (lambda (answer part)
                   `(fiasco:is ,(append (if (and (consp answer)
                                                 (not (eq 'quote (car answer))))
                                            answer
                                            `(equal ,answer))
                                        `((answer ',(find-symbol part package))))))
                 answers
                 '("PART1" "PART2")))))

(defun to-array (input)
  (when (listp input)
    (setf input (str:join #\Newline input)))
  (loop with lines = (aoc:lines input)
        with array = (make-array (list (length lines) (length (first lines))))
        for i from 0
        for c across (remove #\Newline input)
        do (setf (row-major-aref array i) c)
        finally (return array )))

(defun tr (set1 set2 seq)
  (map (type-of seq)
       (lambda (x)
         (let ((pos (position x set1)))
           (if pos
               (elt set2 pos)
               x)))
       seq))

(defun symbols (symbols)
  (lambda (s) (find s symbols :key #'symbol-name :test #'string-equal)))

(defun digits (n)
  (length (format nil "~d" n)))

(defun getfmt (n)
  (format nil "~~~d,d" (digits n)))

(defun getspc (n)
  (format nil "~{~a~}"
          (make-list (digits n) :initial-element #\Space)))

(defun top-row (rows cols i &optional (from 0))
  (with-output-to-string (str)
    (format str "~&~a" (getspc rows))
    (loop with fmt = (getfmt (+ cols from))
          for n below cols
          for s = (format nil fmt (abs (+ from n)))
          for c = (aref s i)
          do (princ c str))))

(defun print-indexed-lines (lines &key cols (row+ 0) (col+ 0))
  (let ((rows (length lines)))
    (unless cols
      (setf cols (reduce #'max (mapcar #'length lines))))
    (loop for i from 0 below (digits (+ cols col+)) do
      (princ (top-row (+ rows row+) cols i col+)) (terpri))
    (loop with fmt = (getfmt rows)
          for l in lines
          for n from row+
          do (format t "~&")
             (format t fmt (abs n))
             (princ l))))

(defun print-array (array &key (row+ 0) (col+ 0) (map #'identity))
  (print-indexed-lines
   (loop for y below (array-dimension array 0)
         collect (with-output-to-string (s)
                   (loop for x below (array-dimension array 1)
                         do (princ (funcall map (aref array y x)) s))))
   :cols (array-dimension array 1)
   :row+ row+ :col+ col+))

(defun in-line? (p from to)
  (destructuring-bind (px py) p
    (destructuring-bind (fx fy) from
      (destructuring-bind (tx ty) to
        (cond ((= fx tx)
               (and (= fx px) (<= (min fy ty) py (max fy ty))))
              ((= fy ty)
               (and (= fy py) (<= (min fx tx) px (max fx tx))))
              ((= (abs (- fx tx)) (abs (- fy ty)))
               (and (= (abs (- fx px)) (abs (- fy py)))
                    (= (abs (- tx px)) (abs (- ty py)))
                    (<= (min fx tx) px (max fx tx))
                    (<= (min fy ty) py (max fy ty)))))))))

(defmacro comparisons (a b (&optional (predicate '<) (test 'eql)) &rest keys)
  (labels ((cmp (a b keys)
             (when keys
               `(let ((x (,(car keys) ,a))
                      (y (,(car keys) ,b)))
                  (or (,predicate x y)
                      (and (,test x y)
                           ,(cmp a b (rest keys))))))))
    (a:once-only (a b)
      (cmp a b keys))))
