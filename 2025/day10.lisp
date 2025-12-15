;;;; day10.lisp

(in-package :aoc2025.day10)

(defun parse (input &key (with-joltage-requirements nil))
  (let ((with-indicator-lights (mapcar #'aoc:read-as-list (aoc:lines (aoc:tr "[],{}" "\"\" ()" input)))))
    (mapcar (if with-joltage-requirements (a:compose #'reverse #'rest) #'butlast) with-indicator-lights)))

(defun toggle (lights button)
  (setf lights (a:copy-array lights))
  (dolist (i button lights)
    (setf (aref lights i) (ecase (aref lights i) (#\. #\#) (#\# #\.)))))

(defun all-off (goal)
  (make-string (length goal) :initial-element #\.))

(defun neighbours (buttons)
  (lambda (lights)
    (mapcar (lambda (button) (toggle lights button)) buttons)))

(defun presses (machine)
  (destructuring-bind (goal . buttons) machine
    (dijkstra:distance (dijkstra:search* (all-off goal) (neighbours buttons)
                                         :goal goal :comparef #'string=))))
(defun part1 (input)
  (reduce #'+ (mapcar #'presses (parse input))))

(defun symbols (buttons)
  (loop for i from (char-code #\A)
        repeat (length buttons)
        collect (intern (format nil "~a" (code-char i)))))

(defun objective (symbols)
  `(min (= steps (+ ,@symbols))))

(defun joltage-constraints (goal buttons symbols)
  (flet ((symbols (i)
           (loop for s in symbols
                 for indices in buttons
                 when (find i indices)
                   collect s)))
    (loop for i from 0
          for x in goal
          collect `(= (+ ,@(symbols i)) ,x))))

(defun integer-constraints (symbols)
  `((integer ,@symbols)))

(defun problem (machine)
  (destructuring-bind (goal . buttons) machine
    (let ((symbols (symbols buttons)))
      (lp:parse-linear-problem (objective symbols)
                               (append (joltage-constraints goal buttons symbols)
                                       (integer-constraints symbols))))))

(defun presses* (machine)
  (truncate (ceiling (lp:solution-objective-value (lp:solve-problem (problem machine))))))

(defun part2 (input)
  (if (asdf:component-loaded-p :linear-programming-glpk)
      (let ((linear-programming:*solver* (find-symbol "GLPK-SOLVER" :linear-programming-glpk)))
        (reduce #'+ (mapcar #'presses* (parse input :with-joltage-requirements t))))
      (error "Load linear-programming-glpk and retry.")))
