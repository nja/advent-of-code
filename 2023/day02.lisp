;;;; day02.lisp

(in-package :aoc2023.day02)

(defun games (input)
  (mapcar #'parse-line (aoc:lines input)))

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
  (or (car (rassoc color bag)) 0))

(defun valid-draw? (bag draw)
  (loop for (count . color) in draw
        always (<= count (cube-count bag color))))

(defun part1 (input)
  (reduce #'+ (mapcar #'id (remove-if-not (a:curry #'possible? '((12 . red) (13 . green) (14 . blue)))
                                          (games input)))))

(defun colors (game)
  (remove-duplicates (mapcan (lambda (draw) (mapcar #'cdr draw))
                             (draws game))))

(defun color-maximum (game color)
  (reduce #'max (mapcar (a:rcurry #'cube-count color) (draws game))))

(defun minimum-set (game)
  (mapcar (lambda (color)
            (cons (color-maximum game color) color))
          (colors game)))

(defun power (bag)
  (reduce #'* (mapcar #'car bag)))

(defun part2 (input)
  (reduce #'+ (mapcar (a:compose #'power #'minimum-set) (games input))))
