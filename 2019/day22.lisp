;;;; day22.lisp

(in-package :aoc2019.day22)

(defun parse (input)
  (mapcar (lambda (line)
            (cons (intern (string-upcase (aoc:tr " " "-" (str:trim (ppcre:scan-to-strings "^[a-z ]* " line))))
                          (symbol-package 'parse))
                  (aoc:read-integers line)))
          (aoc:lines input)))

(defun deck (&optional (n 10007))
  (let ((deck (make-array n)))
    (dotimes (i n deck)
      (setf (aref deck i) i))))

(defun deal-into-new (deck)
  (nreverse deck))

(defparameter *tmp* (deck))

(defun cut (deck n)
  (let ((i (if (plusp n) n (- (length deck) (abs n)))))
    (replace *tmp* deck :start2 i)
    (replace *tmp* deck :start1 (- (length deck) i))
    (rotatef *tmp* deck)
    deck))

(defun deal-with-increment (deck n)
  (loop for i = 0 then (mod (+ i n) (length deck))
        for x across deck
        do (setf (aref *tmp* i) x))
  (rotatef *tmp* deck)
  deck)

(defun shuffle (deck shuffles)
  (reduce (lambda (deck shuffle) (apply (first shuffle) deck (rest shuffle)))
          shuffles :initial-value deck))

(defun part1 (input)
  (position 2019 (shuffle (deck) (parse input))))

(defun compose (cards shuffles)
  (values-list
   (reduce (lambda (coeffs shuffle)
             (destructuring-bind (a c) coeffs
               (destructuring-bind (technique &optional n) shuffle
                 (destructuring-bind (ta tc) (case technique
                                               (cut (list 1 (- n)))
                                               (deal-into-new (list -1 -1))
                                               (deal-with-increment (list n 0)))
                   (list (* a ta) (+ (* c ta) tc))))))
           shuffles :initial-value '(1 0))))

(defun inverse (cards a c)
  (let ((a (maths:invmod a cards)))
    (values a (- cards (* a c)))))

(defun power (cards power a c)
  (let ((ap (maths:exptmod a power cards)))
    (values ap (* (1- ap) (maths:invmod (1- a) cards) c))))

(defun solve (shuffles cards times index)
  (multiple-value-bind (a c) (compose cards shuffles)
    (multiple-value-bind (a c) (inverse cards a c)
      (multiple-value-bind (a c) (power cards times a c)
        (mod (+ (* a index) c) cards)))))

(defun part2 (input)
  (solve (parse input) 119315717514047 101741582076661 2020))
