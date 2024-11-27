;;;; day14.lisp

(in-package :aoc2019.day14)

(defparameter *test*
"10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")

(defparameter *test2*
"9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL")

(defun parse (input)
  (flet ((read-plist (s)
           (reverse (read-from-string (format nil "(~a)" (aoc:tr "," "  " s))))))
    (mapcar (lambda (line)
              (mapcar #'read-plist (str:split " => " line)))
            (aoc:lines input))))

(defun costs-lookup (costs)
  (loop with hash = (make-hash-table)
        for entry in costs
        for key = (caadr entry)
        do (setf (gethash key hash) entry)
        finally (return hash)))

(defun add (bag stuff)
  (cond ((null stuff) bag)
        (t (incf (gethash (first stuff) bag 0) (second stuff))
           (add bag (cddr stuff)))))

(defun take (bag stuff)
  (cond ((null stuff) bag)
        (t (assert (<= 0 (decf (gethash (first stuff) bag 0) (second stuff))))
           (take bag (cddr stuff)))))

(defun have? (bag stuff)
  (or (null stuff)
      (and (<= (second stuff) (gethash (first stuff) bag 0))
           (have? bag (cddr stuff))
           bag)))

(defun add-unstocked (stock needs stuff)
  (loop for (mat amount) on stuff by #'cddr
        unless (have? stock (list mat amount))
          do (add needs (list mat amount))))

(defclass state ()
  ((stock :reader stock :initform (make-hash-table))
   (needs :reader needs :initform (add (make-hash-table) '(fuel 1)))
   (costs :accessor costs :initarg :costs)
   (mats :accessor mats :initarg :mats)
   (spent :accessor spent :initform 0)))

(defun make-state (costs)
  (make-instance 'state
                 :costs costs
                 :mats (a:hash-table-keys costs)))

(defun process (state)
  (with-slots (stock needs mats costs) state
    (dolist (mat mats state)
      (when (gethash mat needs)
        (destructuring-bind (cost amount) (gethash mat costs)
          (cond ((have? stock cost)
                 (take stock cost)
                 (add stock amount)
                 (clrhash needs)
                 (add-unstocked stock needs '(fuel 1))
                 ;(format t "~a => ~a~%" cost amount)
                 (return-from process state)
                 )
                (t
                 (add-unstocked stock needs cost))))))))

(defun extract (state)
  (with-slots (stock needs spent) state
    (let ((n (gethash 'ore needs 0)))
      (when (plusp n)
       (incf (gethash 'ore stock 0))
       (incf spent)
       (setf (gethash 'ore needs) 0))))
  state)

(defun report (state)
  (format t "Stock: ~a~%" (a:hash-table-plist (stock state)))
  (format t "Needs: ~a~%" (a:hash-table-plist (needs state)))
  (format t "Spent: ~d~%" (spent state))
  state)

(defun part1 (input)
  (loop with state = (make-state (costs-lookup (parse input)))
        until (gethash 'fuel (stock state))
        do (extract (process state))
        finally (return (spent state))))

(defparameter *test3*
"157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")

(defun tests ()
  (equalp (print (mapcar #'part1 (list *test* *test2* *test3* *test4* *test5*)))
          (print (list 31 165 13312 180697 2210736))))

(defparameter *test4*
"2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF")

(defparameter *test5*
"171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX")