;;;; day07.lisp

(in-package :aoc2023.day07)

(defun parse (line)
  (list (first (str:split " " line))
        (parse-integer (second (str:split " " line)))))

(defun count-kinds (hand)
  (loop with kinds
        for k across (first hand)
        do (incf (getf kinds k 0))
        finally (return kinds)))

(defun max-of-a-kind (counts)
  (reduce #'max (remove-if-not #'integerp counts)))

(defun hand-type (hand)
  (let ((counts (count-kinds hand)))
    (case (count-if #'characterp counts)
      (1 'five-of-a-kind)
      (2 (case (max-of-a-kind counts)
           (4 'four-of-a-kind)
           (3 'full-house)))
      (3 (case (max-of-a-kind counts)
           (3 'three-of-a-kind)
           (2 'two-pairs)))
      (4 'one-pair)
      (5 'high-card))))

(defparameter *hand-type* #'hand-type)

(defun hand-strength (hand)
  (position (funcall *hand-type* hand) '(five-of-a-kind four-of-a-kind full-house
                                         three-of-a-kind two-pairs one-pair high-card)))

(defparameter *cards* "AKQJT98765432")

(defun card-strength (card)
  (position card *cards*))

(defun compare-hands (a b)
  (let ((sa (hand-strength a))
        (sb (hand-strength b)))
    (cond ((< sa sb) t)
          ((< sb sa) nil)
          (t (loop for ca across (first a)
                   for cb across (first b)
                   for sa = (card-strength ca)
                   for sb = (card-strength cb)
                   if (< sa sb)
                     return t
                   if (< sb sa)
                     return nil)))))

(defun rank-hands (hands)
  (reverse (sort hands #'compare-hands)))

(defun total-winnings (hands)
  (loop for rank from 1
        for hand in (rank-hands hands)
        sum (* rank (second hand))))

(defun part1 (input)
  (total-winnings (mapcar #'parse (aoc:lines input))))

(defun joker-type (hand)
  (let ((type (hand-type hand))
        (jokers (count #\J (first hand))))
    (or (case type
          (four-of-a-kind (case jokers ((4 1) 'five-of-a-kind)))
          (full-house (case jokers ((3 2) 'five-of-a-kind)))
          (three-of-a-kind (case jokers
                             (3 'four-of-a-kind)
                             (1 'four-of-a-kind)))
          (two-pairs (case jokers
                       (2 'four-of-a-kind)
                       (1 'full-house)))
          (one-pair (case jokers ((2 1) 'three-of-a-kind))))
        type)))

(defun part2 (input)
  (let ((*cards* "AKQT98765432J")
        (*hand-type* #'joker-type))
    (total-winnings (mapcar #'parse (aoc:lines input)))))

;;; 253262423 too low
;;; 253154990 too low

(defparameter *test*
  "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")