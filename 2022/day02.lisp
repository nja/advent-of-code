;;;; day02.lisp

(in-package #:aoc2022.day02)

(defparameter *parser* nil)

(defun parse (line)
  (sublis *parser* (let ((*package* (symbol-package 'parse)))
                     (read-from-string (format nil "(~a)" line)))))

(defun winner (x)
  (getf (a:circular-list 'rock 'paper 'scissors) x))

(defun loser (x)
  (winner (winner x)))

(defun result (a b)
  (cond ((eq a b) 'draw)
        ((eq a (loser b)) 'win)
        ((eq a (winner b)) 'loss)))

(defun score (game)
  (+ (getf '(win 6 draw 3 loss 0) (apply #'result game))
     (getf '(rock 1 paper 2 scissors 3) (second game))))

(defun part1 (input)
  (let ((*parser* '((a . rock) (b . paper) (c . scissors)
                    (x . rock) (y . paper) (z . scissors))))
    (reduce #'+ (mapcar (a:compose #'score #'parse) (aoc:lines input)))))

(defun strategy (game)
  (destructuring-bind (a b) game
    (list a (funcall b a))))

(defun part2 (input)
  (let ((*parser* '((a . rock) (b . paper) (c . scissors)
                    (x . loser) (y . identity) (z . winner))))
    (reduce #'+ (mapcar (a:compose #'score #'strategy #'parse) (aoc:lines input)))))
