;;;; day07.lisp

(in-package :aoc2023.day07)

(defun hands (input)
  (mapcar (lambda (l) (list (first (str:split " " l))
                            (parse-integer (second (str:split " " l)))))
          (aoc:lines input)))

(defun cards (hand) (first hand))
(defun bid (hand) (second hand))

(defun count-kinds (hand)
  (loop with kinds
        for k across (cards hand)
        do (incf (getf kinds k 0))
        finally (return kinds)))

(defun max-of-a-kind (counts)
  (reduce #'max (remove-if-not #'integerp counts)))

(defun kinds-of-cards (counts)
  (count-if #'characterp counts))

(defun hand-type (hand)
  (let ((counts (count-kinds hand)))
    (case (kinds-of-cards counts)
      (1 'five-of-a-kind)
      (2 (case (max-of-a-kind counts)
           (4 'four-of-a-kind)
           (3 'full-house)))
      (3 (case (max-of-a-kind counts)
           (3 'three-of-a-kind)
           (2 'two-pairs)))
      (4 'one-pair)
      (5 'high-card))))

(defparameter *cards* "AKQJT98765432")
(defparameter *hands* '(five-of-a-kind four-of-a-kind full-house
                        three-of-a-kind two-pairs one-pair high-card))
(defparameter *joker* nil)

(defun hand-strength (hand)
  (position (joker-type hand) *hands*))

(defun card-strength (card)
  (if (eql card *joker*)
      (length *cards*)
      (position card *cards*)))

(defun compare-hands (a b)
  (or (< (hand-strength a) (hand-strength b))
      (and (not (< (hand-strength b) (hand-strength a)))
           (compare-cards (cards a) (cards b)))))

(defun compare-cards (a b)
  (loop for i below (length a)
        for sa = (card-strength (aref a i))
        for sb = (card-strength (aref b i))
        if (< sa sb)
          return t
        if (< sb sa)
          return nil))

(defun rank-hands (hands)
  (sort (copy-list hands) #'compare-hands))

(defun total-winnings (hands)
  (loop for rank from 1
        for hand in (reverse (rank-hands hands))
        sum (* rank (bid hand))))

(defun part1 (input)
  (total-winnings (hands input)))

(defun joker-type (hand)
  (let ((type (hand-type hand))
        (jokers (count *joker* (cards hand))))
    (or (case type
          (four-of-a-kind (case jokers ((4 1) 'five-of-a-kind)))
          (full-house (case jokers ((3 2) 'five-of-a-kind)))
          (three-of-a-kind (case jokers ((3 1) 'four-of-a-kind)))
          (two-pairs (case jokers
                       (2 'four-of-a-kind)
                       (1 'full-house)))
          (one-pair (case jokers ((2 1) 'three-of-a-kind)))
          (high-card (case jokers (1 'one-pair))))
        type)))

(defun part2 (input)
  (let ((*joker* #\J))
    (total-winnings (hands input))))
