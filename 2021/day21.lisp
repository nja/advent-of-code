;;;; day21.lisp

(in-package #:aoc2021.day21)

(defun player (position) (cons 0 position))
(defun score (player) (car player))

(defun parse-players (input)
  (mapcar (lambda (line) (player (parse-integer line :start 28)))
          (aoc:lines input)))

(defun move (player steps)
  (symbol-macrolet ((score (car player)) (pos (cdr player)))
    (setf pos (wrap-to-1 (+ pos steps) 10))
    (incf score pos)))

(defun wrap-to-1 (x max)
  (1+ (mod (1- x) max)))

(defun die ()
  (let ((x 0))
    (flet ((roll () (wrap-to-1 (incf x) 100)))
      (lambda () (+ (roll) (roll) (roll))))))

(defun game (player other)
  (loop with die = (die)
        for rolls from 0 by 3
        while (< (score other) 1000)
        do (move player (funcall die))
           (rotatef player other)
        finally (return (list rolls player))))

(defun part1 (input)
  (destructuring-bind (rolls loser) (apply #'game (parse-players input))
    (* rolls (score loser))))

(defun distribution (n dice)
  (let* ((sides (loop for i from 1 upto n collect i))
         (rolls (apply #'a:map-product #'list (loop repeat dice collect sides)))
         (sums (mapcar (a:curry #'reduce #'+) rolls)))
    (mapcar (lambda (sum) (list sum (count sum sums)))
            (remove-duplicates sums))))

(defparameter *distribution* (distribution 3 3))

(defun quantum-game (score pos other-score other-pos)
  (loop with wins = 0 and losses = 0
        for (roll times) in *distribution*
        for new-pos = (wrap-to-1 (+ pos roll) 10)
        for new-score = (+ score new-pos)
        if (>= new-score 21)
          do (incf wins times)
        else
          do (destructuring-bind (qlosses qwins)
                 (quantum-game other-score other-pos new-score new-pos)
               (incf wins (* times qwins))
               (incf losses (* times qlosses)))
        finally (return (list wins losses))))

(fare-memoization:memoize 'quantum-game)

(defun part2 (input)
  (destructuring-bind ((score . pos) (other-score . other-pos)) (parse-players input)
    (apply #'max (quantum-game score pos other-score other-pos))))
