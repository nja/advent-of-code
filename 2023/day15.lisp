;;;; day15.lisp

(in-package :aoc2023.day15)

(defparameter *test* "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defun hash (string)
  (loop with hash fixnum = 0
        for char across string
        for code = (char-code char)
        do (setf hash (mod (* 17 (+ hash code)) 256))
        finally (return hash)))

(defun part1 (input)
  (reduce #'+ (mapcar #'hash (str:split "," (remove #\Newline input)))))

(defun parse (input)
  (mapcar (lambda (item)
            (ppcre:register-groups-bind (a ((aoc:symbols '(- =)) b) (#'parse-integer c))
                ("(\\w+)([-=])(\\d+)?" item)
              (list a b c)))
          (str:split "," input)))

(defun make-boxes ()
  (make-array 256 :initial-element nil))

(defun perform-step (step boxes)
  (let ((i (box step)))
    (symbol-macrolet ((box (aref boxes i)))
      (case (second step)
        (- (setf box (remove (label step) box :test #'string= :key #'label)))
        (= (let ((lens (find (label step) box :test #'string= :key #'label)))
             (if lens
                 (setf (second lens) (focal-length step))
                 (setf box (nconc box (list (list (label step) (focal-length step))))))))))))

(defun label (x) (first x))

(defun box (step)
  (hash (label step)))

(defun focal-length (step)
  (third step))

(defun focusing-power (boxes)
  (loop for box-number from 1
        for lenses across boxes
        sum (loop for lens in lenses
                  for slot-number from 1
                  sum (* box-number slot-number (second lens)))))

(defun configure-lenses (steps)
  (let ((boxes (make-boxes)))
    (dolist (step steps boxes)
      (perform-step step boxes))))

(defun part2 (input)
  (focusing-power (configure-lenses (parse input))))