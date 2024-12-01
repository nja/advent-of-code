;;;; day04.lisp

(in-package #:aoc2021.day04)

(defun parse (input)
  (destructuring-bind (draw-line &rest board-sections) (aoc:sections input)
    (list (parse-integers draw-line)
          (mapcar #'parse-board board-sections))))

(defun parse-board (section)
  (make-array '(5 5) :initial-contents (mapcar #'parse-integers (aoc:lines section))))

(defun parse-integers (line)
  (mapcar #'parse-integer (ppcre:split "\\D+" (string-trim '(#\Space) line))))

(defun mark (n board)
  (loop for i below (array-total-size board)
        when (eql n (row-major-aref board i))
          do (setf (row-major-aref board i) (list n))
        when (bingo? board) return board))

(defun bingo? (board)
  (loop for a below 5
          thereis (loop for b below 5
                        count (marked (aref board a b)) into row
                        count (marked (aref board b a)) into col
                        finally (return (or (= row 5) (= col 5))))))

(defun marked (x) (when (consp x) (car x)))
(defun unmarked (x) (when (integerp x) x))

(defun unmarked-sum (board)
  (loop for i below (array-total-size board)
        when (unmarked (row-major-aref board i)) sum it))

(defun turn (n boards)
  (car (remove-if-not (a:curry #'mark n) boards)))

(defun bingo (draw boards)
  (loop for n in draw
        for b = (turn n boards)
        when b return (list b n)))

(defun score (board n)
  (* (unmarked-sum board) n))

(defun part1 (input)
  (apply #'score (apply #'bingo (parse input))))

(defun turns-played (board)
  (loop for i below (array-total-size board)
        count (marked (row-major-aref board i))))

(defun loser (draw boards)
  (first (sort (mapcar (lambda (b) (bingo draw (list b))) boards)
               #'> :key (a:compose #'turns-played #'first))))

(defun part2 (input)
  (apply #'score (apply #'loser (parse input))))
