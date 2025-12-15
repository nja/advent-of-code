;;;; day11.lisp

(in-package :aoc2025.day11)

(defun parse (input)
  (mapcar #'aoc:read-as-list (aoc:lines (aoc:tr ":" " " input))))

(defun as-hash-table (list)
  (let ((hash-table (make-hash-table)))
    (dolist (x list hash-table)
      (setf (gethash (first x) hash-table) (rest x)))))

(defun ends (connections start)
  (mapcar (a:compose #'first #'dijkstra:item)
          (dijkstra:search* (list start nil) (neighbours connections))))

(defun neighbours (connections)
  (lambda (node)
    (destructuring-bind (at path) node
      (mapcar (lambda (neighbour) (list neighbour (cons at path)))
              (gethash at connections)))))

(defun part1 (input)
  (count 'out (ends (as-hash-table (parse input)) 'you)))

(defun count-paths (neighboursf start end)
  (let ((counts (make-hash-table)))
    (labels ((count-from (node)
               (if (eql node end)
                   1
                   (or (gethash node counts)
                       (setf (gethash node counts)
                             (reduce #'+ (mapcar #'count-from (funcall neighboursf node))))))))
      (count-from start))))

(defun part2 (input)
  (let* ((neighbours (a:rcurry #'gethash (as-hash-table (parse input)))))
    (* (count-paths neighbours 'svr 'fft)
       (count-paths neighbours 'fft 'dac)
       (count-paths neighbours 'dac 'out))))
