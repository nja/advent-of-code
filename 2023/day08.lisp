;;;; day08.lisp

(in-package :aoc2023.day08)

(setf *print-circle* t)

(defun parse-instructions (input)
  (map 'list (lambda (c) (case c (#\L 'L) (#\R 'R))) (first (aoc:sections input))))

(defun parse-map (input)
  (mapcar (lambda (line)
            (mapcar (lambda (s) (intern s (symbol-package 'parse-map)))
                    (ppcre:all-matches-as-strings "[A-Z0-9]+" line)))
          (aoc:lines (second (aoc:sections input)))))

(defun hash-map (map)
  (let ((hash-table (make-hash-table)))
    (dolist (entry map hash-table)
      (setf (gethash (car entry) hash-table) (cdr entry)))))

(defun walk (instructions hash-map)
  (loop with directions = (apply #'a:circular-list instructions)
        for dir in directions
        for steps from 1
        for (l r) = (gethash 'AAA hash-map) then (gethash position hash-map)
        for position = (case dir (l l) (r r))
        when (eql position 'ZZZ)
          return steps))

(defun part1 (input)
  (walk (parse-instructions input) (hash-map (parse-map input))))

(defun walker (hash-map ends position instructions)
  (let (done (directions (apply #'a:circular-list instructions)))
    (lambda (s)
      (or done 
          (destructuring-bind (l r) (gethash position hash-map)
            (setf position (case (car directions) (l l) (r r)))
            (setf directions (cdr directions))
            (when (gethash position ends)
              (setf done s)))))))

(defun ending-with (e map)
  (remove-if-not (lambda (symbol) (str:ends-with-p e (symbol-name symbol)))
                 (mapcar #'car map)))

(defun set-map (list)
  (let ((hash-table (make-hash-table)))
    (dolist (x list hash-table)
      (setf (gethash x hash-table) t))))

(defun walkers (walkers)
  (loop for steps from 1
        for results = (mapcar (lambda (w) (funcall w steps)) walkers)
        when (every #'integerp results)
          return (reduce #'lcm results)))

(defun part2 (input)
  (let* ((map (parse-map input)))
    (walkers (mapcar (lambda (p) (walker (hash-map map) (set-map (ending-with "Z" map)) p (parse-instructions input)))
                     (ending-with "A" map)))))
