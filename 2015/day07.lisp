;;;; day07.lisp

(in-package :aoc2015.day07)

(defun parse (input)
  (let ((*package* (symbol-package 'parse)))
    (mapcar (lambda (line) (remove '-> (read-from-string (format nil "(~a)" line))))
            (aoc:lines input))))

(defun circuit (instructions)
  (let ((circuit (make-hash-table)))
    (dolist (instruction instructions circuit)
      (ecase (length instruction)
        (2 (destructuring-bind (a name) instruction
             (set-signal circuit name a)))
        (3 (destructuring-bind (op a name) instruction
             (set-signal circuit name (gate op a))))
        (4 (destructuring-bind (a op b name) instruction
             (set-signal circuit name (gate op a b))))))))

(defun gate (op a &optional b)
  (lambda (circuit)
    (flet ((a () (get-signal circuit a))
           (b () (get-signal circuit b)))
      (case op
        (or (logior (a) (b)))
        (and (logand (a) (b)))
        (not (lognot (a)))
        (lshift (logand #xffff (ash (a) (b))))
        (rshift (ash (a) (- (b))))))))

(defun set-signal (circuit name thing)
  (setf (gethash name circuit) thing))

(defun get-signal (circuit source)
  (if (integerp source)
      source
      (let ((thing (gethash source circuit)))
        (etypecase thing
          (integer thing)
          (function (setf (gethash source circuit) (funcall thing circuit)))
          (symbol (setf (gethash source circuit) (get-signal circuit thing)))))))

(defun part1 (input)
  (get-signal (circuit (parse input)) 'a))

(defun part2 (input)
  (let ((circuit (circuit (parse input))))
    (set-signal circuit 'b (part1 input))
    (get-signal circuit 'a)))
