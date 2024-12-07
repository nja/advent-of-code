;;;; day05.lisp

(in-package #:aoc2022.day05)

(defun parse-stacks (section)
  (let ((layers (rest (reverse (aoc:lines section))))
        (stacks (make-array 10 :initial-element nil)))
    (dolist (layer layers stacks)
      (loop for i from 0
            for c across layer
            when (alpha-char-p c)
              do (push c (aref stacks (1+ (truncate i 4))))))))

(defun parse-moves (section)
  (mapcar (lambda (l)
            (aoc:read-as-list (ppcre:regex-replace-all "[^0-9]" l " ")))
          (aoc:lines section)))

(defun move (stacks n from to)
  (dotimes (i n)
    (push (pop (aref stacks from)) (aref stacks to))))

(defun rearrange (move stacks moves)
  (dolist (m moves stacks)
    (apply move stacks m)))

(defun as-string (stacks)
  (map 'string #'first (remove nil stacks)))

(defun part1 (input)
  (as-string (rearrange #'move
                        (parse-stacks (first (aoc:sections input)))
                        (parse-moves (second (aoc:sections input))))))

(defun move2 (stacks n from to)
  (move stacks n from 0)
  (move stacks n 0 to))

(defun part2 (input)
  (as-string (rearrange #'move2
                        (parse-stacks (first (aoc:sections input)))
                        (parse-moves (second (aoc:sections input))))))
