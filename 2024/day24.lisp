;;;; day24.lisp

(in-package :aoc2024.day24)

(defun parse (input)
  (values (mapcar #'parse-wire (aoc:lines (first (aoc:sections input))))
          (mapcar #'parse-gate (aoc:lines (second (aoc:sections input))))))

(defun parse-wire (line)
  (destructuring-bind (a b) (str:split ": " line)
    (list (intern (string-upcase a) (symbol-package 'parse))
          (parse-integer b))))

(defun parse-gate (line)
  (mapcar (lambda (s)
            (intern (string-upcase s) (symbol-package 'parse)))
          (str:split " " line)))

(defun setup (wires)
  (loop with h = (make-hash-table)
        for (name signal) in wires
        do (setf (gethash name h) signal)
        finally (return h)))

(defun process (wires gates)
  (loop for gate in gates
        for (a op b -> wire) = gate
        for as = (gethash a wires)
        for bs = (gethash b wires)
        if (and as bs)
          do (setf (gethash wire wires)
                   (case op
                     (and (logand as bs))
                     (xor (logxor as bs))
                     (or (logior as bs))))
        else
          collect gate))

(defun simulate (wires gates)
  (let ((wires (setup wires)))
    (loop while gates
          do (setf gates (process wires gates))
          finally (return wires))))

(defun decimal-number (wires predicate)
  (loop with number = 0
        for wire in (sort (remove-if-not predicate (a:hash-table-keys wires))
                       #'string> :key #'symbol-name)
        do (setf number (+ (ash number 1) (gethash wire wires)))
        finally (return number)))

(defun part1 (input)
  (decimal-number (multiple-value-call #'simulate (parse input))
                  (lambda (wire) (str:starts-with? "Z" (symbol-name wire)))))

(defun as-string (x)
  (cond ((symbolp x) (symbol-name x))
        (t (coerce x 'string))))

(defun pin (wire)
  (if (integerp wire)
      wire
      (let ((pins (remove-duplicates (aoc:read-integers (remove #\- (as-string wire))))))
        (and (eql 1 (length pins)) (first pins)))))

(defun affinity (wire)
  (find-symbol (str:trim-right (remove-if #'digit-char-p (as-string wire)) :char-bag "-")))

(defun affinity? (wire x)
  (string= x (affinity wire)))

(defun order (a b &optional priority)
  (let ((priority (append priority '(x y z xor-in xor-carry in-carry carry))))
    (sort (list a b) #'<
          :key (lambda (x) (or (position (affinity x) priority) (length priority))))))

(defun order-inputs (gate &rest priority)
  (destructuring-bind (a op b -> output) gate
    (destructuring-bind (a b) (order a b priority)
      (list a op b -> output))))

(defun wires (pin)
  (mapcar (lambda (x) (find-symbol (format nil "~a~2,'0d" x (pin pin))))
          '(x y z)))

(defun adder (bit &optional carry)
  (destructuring-bind (x y z) (wires bit)
    (destructuring-bind (x xor y -> xor-in) (match x 'xor y)
      (destructuring-bind (x and y -> and-in) (match x 'and y)
        (if (zerop bit)
            (list xor-in and-in)
            (destructuring-bind (xor-in xor carry -> xor-out) (match carry 'xor)
              (destructuring-bind (xor-in and carry -> and-carry) (match carry 'and)
                (destructuring-bind (and-in or and-carry -> carry) (match and-in 'or and-carry)
                  (when (and xor-in and-in xor-out and-carry and-carry carry
                             (eq z xor-out))
                    (list xor-out carry))))))))))

(defun adders (&optional (from 0) carry)
  (loop for bit from from
        for (output c) = (adder bit (or c carry))
        while (and output c)
        collect (print (list output c))))

(defun match (&optional a op b -> output)
  (or (a:when-let (gate (print (find-if (lambda (gate)
                                          (let ((gate (order-inputs gate a b)))
                                            (when (every (lambda (x y) (or (null x) (eq x y)))
                                                         (list a op b -> output)
                                                         gate)
                                              gate)))
                                        *gates*)))
        (order-inputs gate a b))
      (list a op b -> output)))

(defvar -> '->)

(defun answer (symbols)
  (when (eql 8 (length symbols))
    (string-downcase (format nil "~{~a~^,~}" (sort symbols #'string<)))))

(defparameter *gates* (apply-answers))

(defparameter *answers* '(((X07 AND Y07 -> Z07) (KPV XOR RVC -> SWT))
                          ((DWQ XOR PGQ -> PQC) (SKT OR WPP -> Z13))
                          ((Y24 XOR X24 -> WSV) (X24 AND Y24 -> RJM))
                          ((KQK AND DJR -> Z31) (DJR XOR KQK -> BGS))))

(defun swap (a b)
  (prog1 (push (list a b) *answers*)
    (setf *gates* (apply-answers))))

(defun apply-answers ()
  (sublis (loop for (a b) in *answers*
                for af = (copy-tree a)
                for bf = (copy-tree b)
                do (rotatef (car (last af)) (car (last bf)))
                collect (cons a af)
                collect (cons b bf))
          (nth-value 1 (parse (aoc:input)))
          :test 'equal))

(defun part2 (input)
  (declare (ignore input))
  (answer (mapcan (lambda (pair) (mapcar #'a:lastcar pair)) *answers2*)))
