;;;; day11.lisp

(in-package #:aoc2016.day11)

(defun parse (input)
  (let ((floors (mapcar (a:compose #'sort-floor #'parse-line) (aoc:lines input))))
    (values (apply #'vector (cons 1 floors))
            (vector 4 nil nil nil (sort-floor (apply #'append floors))))))

(defun parse-line (line)
  (append (parse-things 'chip "a (\\w+)-compatible microchip" line)
          (parse-things 'generator "a (\\w+) generator" line)))

(defun sort-floor (floor)
  (sort (copy-list floor)
        (flet ((thing (x) (symbol-name (car x)))
               (element (x) (symbol-name (cdr x))))
          (lambda (a b)
            (aoc:comparisons a b (string<) element thing)))))

(defparameter *elements* '(hydrogen lithium thulium plutonium strontium promethium ruthenium))

(defun parse-things (type regex line)
  (let (result)
    (ppcre:do-register-groups (((aoc:symbols *elements*) element)) (regex line)
      (push (cons type element) result))
    result))

(defun safe? (floor)
  (not (and (generators? floor) (unpowered-chips? floor))))

(defun generators? (floor)
  (some (lambda (x) (eq 'generator (car x))) floor))

(defun powered? (element floor)
  (some (lambda (x)
          (and (eq 'generator (car x))
               (eq element (cdr x))))
        floor))

(defun unpowered-chips? (floor)
  (some (lambda (x)
          (and (eq 'chip (car x))
               (not (powered? (cdr x) floor))))
        floor))

(defun elevator-load-options (floor)
  (nconc (pairs floor)
         (singles floor)))

(defun singles (floor)
  (mapcar #'list floor))

(defun pairs (floor)
  (if (null floor)
      nil
      (nconc (mapcar (lambda (x)
                       (list (car floor) x))
                     (cdr floor))
             (pairs (cdr floor)))))

(defun remove-from-floor (what floor)
  (remove-if (a:rcurry #'find what) floor))

(defun destination-floor-positions (state)
  (case (aref state 0)
    (1 '(2))
    (2 '(1 3))
    (3 '(2 4))
    (4 '(3))))

(defun neighbours (state)
  (let* ((elevator-position (aref state 0))
         (elevator-floor (aref state elevator-position))
         (loads (elevator-load-options elevator-floor))
         (destination-positions (destination-floor-positions state)))
    (loop for load in loads
          for new-elevator-floor = (remove-from-floor load elevator-floor)
          when (safe? new-elevator-floor)
            append (loop for destination-position in destination-positions
                         for destination-floor = (aref state destination-position)
                         for new-destination-floor = (append load destination-floor)
                         when (safe? new-destination-floor)
                           collect (new-state state
                                              new-elevator-floor
                                              destination-position
                                              new-destination-floor)))))

(defun new-state (state elevator-floor destination-pos destination-floor)
  (let ((copy (copy-seq state))
        (elevator-pos (aref state 0)))
    (setf (aref copy elevator-pos) (sort-floor elevator-floor)
          (aref copy destination-pos) (sort-floor destination-floor)
          (aref copy 0) destination-pos)
    copy))

(defun shortest-path (start goal)
  (d:distance (d:search* start #'neighbours
                         :donep (a:curry #'equalp goal))))

(defun part1 (input)
  (multiple-value-bind (start goal) (parse input)
    (shortest-path start goal)))

(defun patch (state i)
  (let ((extra '((chip . elerium) (chip . dilithium)
                 (generator . elerium) (generator . dilithium))))
    (setf (aref state i) (sort-floor (append extra (aref state i))))
    state))

(defun part2 (input)
  (multiple-value-bind (start goal) (parse input)
    (shortest-path (patch start 1) (patch goal 4))))
