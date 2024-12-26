;;;; day21.lisp

(in-package :aoc2024.day21)

(defun parse (input)
  (aoc:lines input))

(defclass robot ()
  ((keypad :initarg :keypad :reader keypad)
   (position :initform #\A :accessor pos)))

(defun make-keypad (&rest lines)
  (aoc:to-array lines))

(defun numerical-robot ()
  (make-instance 'robot :keypad (make-keypad "789" "456" "123" " 0A")))

(defun directional-robot ()
  (make-instance 'robot :keypad (make-keypad " ^A" "<V>")))

(defun as-char (x)
  (typecase x
    (symbol (let ((name (symbol-name x)))
              (aref name 0)))
    (integer (digit-char x))
    (character x)))

(defun directions (robot key)
  (with-slots (keypad position) robot
    (mapcar #'- (subscripts keypad key) (subscripts keypad position))))

(defun valid-presses? (robot keys)
  (let ((pos (subscripts (keypad robot) (pos robot))))
    (when (loop for key in keys
                do (setf pos (add pos (delta key)))
                never (or (not (apply #'array-in-bounds-p (keypad robot) pos))
                          (equal #\Space (apply #'aref (keypad robot) pos))))
      keys)))

(defun possible-presses (robot key)
  (let (result)
    (a:map-permutations (lambda (keys)
                          (a:when-let (valid (valid-presses? robot keys))
                            (push valid result)))
                        (directions-as-keys (directions robot key)))
    (or (mapcar (lambda (r) (append r '(A))) result)
        '((A)))))

(defun directions-as-keys (directions)
  (append (moves (first directions) '^ 'V)
          (moves (second directions) '< '>)))

(defun moves (n - +)
  (loop repeat (abs n)
        collect (if (plusp n) + -)))

(defun add (a b)
  (mapcar #'+ a b))

(defun delta (key)
  (ecase key
    (^ '(-1  0))
    (v '( 1  0))
    (< '( 0 -1))
    (> '( 0  1))
    (A '( 0  0))))

(defun subscripts (keypad key)
  (let ((key (as-char key)))
    (loop for i below (array-total-size keypad)
          when (equal key (row-major-aref keypad i))
            return (multiple-value-list (truncate i (array-dimension keypad 1))))))

(defun shortest-sequence (n key-presses)
  (let ((key (cons n key-presses)))
    (or (gethash key *memo*)
        (setf (gethash key *memo*)
              (if (zerop n)
                  (length key-presses)
                  (best-presses key-presses (a:curry #'shortest-sequence (1- n))))))))

(defparameter *memo* (make-hash-table :test 'equal))

(defun best-presses (key-presses test &optional (robot (directional-robot)))
  (loop for key in (map 'list #'identity key-presses)
        for possible = (possible-presses robot key)
        sum (a:extremum (mapcar (lambda (keys) (funcall test keys)) possible) #'<)
        do (setf (pos robot) key)))

(defun complexity (n code)
  (* (best-presses code (a:curry #'shortest-sequence n) (numerical-robot))
     (parse-integer code :junk-allowed t)))

(defun robots (n)
  (cons (numerical-robot) (loop repeat n collect (directional-robot))))

(defun part1 (input)
  (reduce #'+ (mapcar (a:curry #'complexity 2) (parse input))))

(defun part2 (input)
  (reduce #'+ (mapcar (a:curry #'complexity 25) (parse input))))
