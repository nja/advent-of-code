;;;; day08.lisp

(in-package #:aoc2021.day08)

(defun parse-output (line)
  (ppcre:split " " (second (ppcre:split " \\| " line))))

(defun unique-length? (x)
  (find (length x) '(2 4 3 7)))

(defun part1 (input)
  (count-if #'unique-length? (a:flatten (mapcar #'parse-output (aoc:lines input)))))

(defparameter *digits*
  '("abcefg" "cf" "acdeg" "acdfg" "bcdf"
    "abdfg" "abdefg" "acf" "abcdefg" "abcdfg"))

(defparameter *segments* '(#\a #\b #\c #\d #\e #\f #\g))

(defun parse-patterns (line)
  (ppcre:split " " (first (ppcre:split " \\| " line))))

(defun initial-state ()
  (mapcar (a:rcurry #'cons *segments*) *segments*))

(defun find-by-length (length digits)
  (find length digits :key #'length))

(defun complement-segments (segments)
  (remove-if (a:rcurry #'find segments) *segments* ))

(defun exclude-if (state fromp whatp)
  (mapcar (lambda (x)
            (destructuring-bind (s . o) x
              (if (funcall fromp s)
                  (cons s (remove-if whatp o))
                  x)))
          state))

(defun exclude (state segments excluded-options)
  (exclude-if state
              (a:rcurry #'find segments)
              (a:rcurry #'find excluded-options)))

(defun exclude-by-length (state patterns)
  (loop initially (setf new-state state)
        for length in '(2 3 4 7)
        for pattern = (find-by-length length patterns)
        for digit = (find-by-length length *digits*)
        for new-state = (exclude new-state pattern (complement-segments digit))
        finally (return new-state)))

(defun unsolved? (entry)
  (< 2 (length entry)))

(defun solved (state)
  (remove-if #'unsolved? state))

(defun exclude-solved (state)
  (let ((solved (solved state)))
    (if solved
        (exclude-if state
                    (lambda (x) (not (find x solved :key #'car)))
                    (let ((excluded (map 'vector #'second solved)))
                      (a:rcurry #'find excluded)))
        state)))

(defun paired (state)
  (flet ((pairing (state entry)
           (find-if (lambda (x) (and (not (equal (car x) (car entry)))
                                     (= 3 (length x))
                                     (equal (cdr entry) (cdr x))))
                    state)))
    (loop for entry in state
          for pairing = (pairing state entry)
          if pairing
            return (list (list (car entry) (car pairing))
                         (cdr entry)))))

(defun exclude-pairs (state)
  (let ((paired (paired state)))
    (if paired
        (destructuring-bind (segments options) paired
          (exclude-if state
                      (lambda (x) (not (find x segments)))
                      (a:rcurry #'find options)))
        state)))

(defun trim (state)
  (exclude-solved (exclude-pairs state)))

(defun entry-guesses (entry)
  (when (< 2 (length entry))
    (mapcar (a:curry #'list (car entry)) (cdr entry))))

(defun state-guesses (state)
  (mapcan #'entry-guesses state))

(defun solved? (state)
  (every (lambda (x) (= 2 (length x))) state))

(defun contradiction? (state)
  (or (some (lambda (x) (= 1 (length x))) state)
      (let ((solved (solved state)))
        (/= (length solved) (length (remove-duplicates (mapcar #'cadr solved)))))))

(defun solve (state guesses verify back)
  (cond ((contradiction? state) (funcall back))
        ((solved? state) (if (funcall verify state)
                             state
                             (funcall back)))
        ((null guesses) (funcall back))
        (t (solve (apply #'select-option state (first guesses))
                  (rest guesses)
                  verify
                  (lambda () (solve state (rest guesses) verify back))))))

(defun translate (solution segments)
  (sort (map (type-of segments) (a:compose #'cadr (a:rcurry #'assoc solution)) segments)
        #'char<))

(defun select-option (state segment option)
  (mapcar (lambda (x)
            (destructuring-bind (s . o) x
              (cons s (if (eql s segment)
                          (list option)
                          (remove option o)))))
          state))

(defun validator (patterns)
  (let ((truth (sort (copy-list *digits*) #'string<)))
    (lambda (solution)
      (equal truth (sort (mapcar (lambda (x) (translate solution x)) patterns)
                         #'string<)))))

(defun solution (patterns)
  (let ((state (trim (exclude-by-length (initial-state) patterns))))
    (solve state (state-guesses state) (validator patterns) (lambda ()))))

(defun to-digits (signals)
  (parse-integer
   (map 'string
        (lambda (x) (digit-char (position x *digits* :test #'equal)))
        signals)))

(defun decode (solution output)
  (mapcar (a:curry #'translate solution) output))

(defun process (line)
  (to-digits (decode (solution (parse-patterns line)) (parse-output line))))

(defun part2 (input)
  (reduce #'+ (mapcar #'process (aoc:lines input))))
