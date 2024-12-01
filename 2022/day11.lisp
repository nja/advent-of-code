;;;; day11.lisp

(in-package #:aoc2022.day11)

(defstruct (monkey) id items operation divisor destinations inspections)

(defun read-as-list (string)
  (let ((*package* (symbol-package 'read-as-list)))
    (read-from-string (format nil "(~a)" string))))

(defun integers (line)
  (read-as-list (ppcre:regex-replace-all "\\D+" line " ")))

(defun parse-monkey (section)
  (with-input-from-string (in section)
    (flet ((line (&optional c) (when c (peek-char c in) (read-char in))
             (read-line in)))
      (make-monkey :id (first (integers (line)))
                   :items (reverse (integers (line #\:)))
                   :operation (apply #'operation (read-as-list (line #\=)))
                   :divisor (first (integers (line)))
                   :destinations (append (integers (line)) (integers (line)))
                   :inspections 0))))

(defun operation (a op b)
  (declare (ignore a))
  (if (eq b 'old)
      (lambda (old) (funcall op old old))
      (lambda (old) (funcall op old b))))

(defun monkeys (input)
  (map 'vector #'parse-monkey (aoc:sections input)))

(defun destination (monkey item)
  (nth (if (zerop (mod item (monkey-divisor monkey))) 0 1)
       (monkey-destinations monkey)))

(defparameter *simplify* nil)

(defun turn (monkeys id)
  (let ((m (aref monkeys id)))
    (dolist (old (reverse (monkey-items m)))
      (let* ((new (funcall *simplify* (funcall (monkey-operation m) old)))
             (destination (aref monkeys (destination m new))))
        (push new (monkey-items destination))
        (incf (monkey-inspections m))))
    (setf (monkey-items m) nil)))

(defun rounds (monkeys n)
  (dotimes (_ n monkeys)
    (dotimes (i (length monkeys))
      (turn monkeys i))))

(defun monkey-business (monkeys)
  (reduce #'* (sort (map 'list #'monkey-inspections monkeys) #'>) :end 2))

(defun part1 (input)
  (let ((*simplify* (a:rcurry #'truncate 3)))
    (monkey-business (rounds (monkeys input) 20))))

(defun part2 (input)
  (let* ((monkeys (monkeys input))
         (*simplify* (a:rcurry #'mod (reduce #'* monkeys :key #'monkey-divisor))))
    (monkey-business (rounds monkeys 10000))))
