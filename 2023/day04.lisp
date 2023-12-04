;;;; day04.lisp

(in-package :aoc2023.day04)

(defun list-of-numbers (string)
  (read-from-string (format nil "(~a)" string)))

(defun parse-card (line)
  (destructuring-bind (a b) (str:split ":" line)
    (list* (parse-integer (remove-if-not #'digit-char-p a))
           (mapcar #'list-of-numbers (str:split "|" b)))))

(defun parse (input)
  (mapcar #'parse-card (aoc:lines input)))

(defun id (card) (first card))
(defun winning (card) (second card))
(defun numbers-you-have (card) (third card))

(defun count-matches (card)
  (length (intersection (winning card) (numbers-you-have card))))

(defun score (card)
  (floor (expt 2 (1- (count-matches card)))))

(defun part1 (input)
  (reduce #'+ (mapcar #'score (parse input))))

(defun count-cards (cards)
  (let ((counts (make-array (1+ (length cards)) :initial-element 1)))
    (decf (aref counts 0))
    (loop for card in cards
          for count = (aref counts (id card))
          for wins = (count-matches card)
          do (loop repeat wins
                   for i from (1+ (id card))
                   do (incf (aref counts i) count)))
    counts))

(defun part2 (input)
  (reduce #'+ (count-cards (parse input))))
(defparameter *test*
"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")