;;;; day15.lisp

(in-package :aoc2023.day15)

(defun hash (string)
  (reduce (lambda (h c) (mod (* 17 (+ h (char-code c))) 256)) string :initial-value 0))

(defun part1 (input)
  (reduce #'+ (mapcar #'hash (str:split "," (remove #\Newline input)))))

(defun parse (input)
  (mapcar (lambda (item)
            (ppcre:register-groups-bind (a ((aoc:symbols '(- =)) b) (#'parse-integer c))
                ("(\\w+)([-=])(\\d+)?" item)
              (list a b c)))
          (str:split "," input)))

(defun perform-step (step boxes)
  (destructuring-bind (label op focal-length) step
    (let ((i (hash label)))
      (symbol-macrolet ((box (aref boxes i)))
        (case op
          (- (setf box (remove label box :test #'string= :key #'first)))
          (= (let ((lens (find label box :test #'string= :key #'first)))
               (if lens
                   (setf (second lens) focal-length)
                   (push (list label focal-length) box)))))))))

(defun focusing-power (boxes)
  (loop for box-number from 1
        for lenses across boxes
        sum (loop for lens in lenses
                  for slot-number downfrom (length lenses)
                  sum (* box-number slot-number (second lens)))))

(defun configure-lenses (steps)
  (let ((boxes (make-array 256 :initial-element nil)))
    (dolist (step steps boxes)
      (perform-step step boxes))))

(defun part2 (input)
  (focusing-power (configure-lenses (parse input))))
