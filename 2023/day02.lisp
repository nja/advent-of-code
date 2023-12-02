;;;; day02.lisp

(in-package :aoc2023.day02)

(defun parse-line (line)
  (destructuring-bind (id games) (str:split ":" line)
    (cons (parse-integer (remove-if-not #'digit-char-p id))
          (mapcar #'parse-game (str:split ";" games)))))

(defun parse-game (string)
  (mapcar (lambda (s)
            (let ((*package* (find-package :aoc2023.day02)))
              (with-input-from-string (in s)
                (cons (read in) (read in)))))
          (str:split "," string)))

(defun id (game) (first game))
(defun draws (game) (rest game))

(defun possible? (bag game)
  (every (a:curry #'valid-draw? bag) (draws game)))

(defun cube-count (bag color)
  (car (rassoc color bag)))

(defun valid-draw? (bag draw)
  (loop for (count . color) in draw
        always (<= count (cube-count bag color))))

(defun part1 (input)
  (reduce #'+ (mapcar #'id (remove-if-not (a:curry #'possible? '((12 . red) (13 . green) (14 . blue)))
                                          (mapcar #'parse-line (aoc:lines input))))))

(defparameter *test* "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")