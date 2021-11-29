;;;; day13.lisp

(in-package #:aoc2018.day13)

(defvar *tracks* nil)
(defvar *state* nil)
(defvar *carts* nil)

(defun to-array (lines)
  (loop with rows = (length lines)
        with cols = (reduce #'max (mapcar #'length lines))
        with array = (make-array (list rows cols) :initial-element #\Space)
        for y from 0
        for line in lines
        do (loop for x from 0
                 for ch across line
                 do (setf (aref array y x) ch))
        finally (return array)))

(defparameter *dirs* "^>v<")
(defparameter *turns* (alexandria:circular-list 'left 'straight 'right))

(defstruct cart dir y x (turn *turns*) (live t))

(defun cart-char? (ch) (find ch *dirs*))

(defun right (cart) (aref *dirs* (mod (1+ (position (cart-dir cart) *dirs*)) 4)))
(defun left (cart) (aref *dirs* (mod (1- (position (cart-dir cart) *dirs*)) 4)))
(defun straight (cart) (cart-dir cart))

(defun under (cart) (aref *tracks* (cart-y cart) (cart-x cart)))

(defun next-pos (cart)
  (case (cart-dir cart)
    (#\^ (list (1- (cart-y cart)) (cart-x cart)))
    (#\> (list (cart-y cart) (1+ (cart-x cart))))
    (#\v (list (1+ (cart-y cart)) (cart-x cart)))
    (#\< (list (cart-y cart) (1- (cart-x cart))))))

(defun corner (cart)
  (setf (cart-dir cart)
        (funcall
         (case (cart-dir cart)
           ((#\^ #\v) (case (under cart)
                        (#\\ #'left)
                        (#\/ #'right)
                        (t #'cart-dir)))
           ((#\> #\<) (case (under cart)
                        (#\\ #'right)
                        (#\/ #'left)
                        (t #'cart-dir))))
         cart))
  cart)

(defun turn (cart)
  (when (char= #\+ (under cart))
    (setf (cart-dir cart) (funcall (car (cart-turn cart)) cart))
    (setf (cart-turn cart) (cdr (cart-turn cart))))
  cart)

(defun collect-carts (array)
  (loop for y below (array-dimension array 0)
        nconc (loop for x below (array-dimension array 1)
                    for ch = (aref array y x)
                    when (cart-char? ch) collect (make-cart :dir ch :y y :x x))))

(defun copy-tracks (array)
  (loop with new-tracks = (make-array (array-dimensions array))
        for y below (array-dimension array 0)
        do (loop for x below (array-dimension array 1)
                 for ch = (aref array y x)
                 do (setf (aref new-tracks y x)
                          (case ch
                            ((#\> #\<) #\-)
                            ((#\^ #\v) #\|)
                            (t ch))))
        finally (return new-tracks)))

(defun cart-at (pos)
  (destructuring-bind (y x) pos
    (find-if (lambda (c)
               (and (= y (cart-y c))
                    (= x (cart-x c))))
             *carts*)))

(defun kill (cart)
  (setf (cart-live cart) nil))

(defun move (cart)
  (setf (aref *state* (cart-y cart) (cart-x cart)) (under cart))
  (let* ((pos (next-pos cart))
         (there (apply #'aref *state* pos)))
    (cond ((cart-char? there)
           (kill cart)
           (setf (apply #'aref *state* pos)
                 (apply #'aref *tracks* pos))
           (kill (cart-at pos))
           pos)
          (t
           (setf (cart-y cart) (car pos))
           (setf (cart-x cart) (cadr pos))
           (setf (apply #'aref *state* pos) (cart-dir cart))
           nil))))

(defun sort-carts (carts)
  (sort (copy-seq carts)
        (lambda (a b)
          (cond ((< (cart-y a) (cart-y b)) t)
                ((< (cart-y b) (cart-y a)) nil)
                ((< (cart-x a) (cart-x b)) t)))))

(defun part1 (input)
  (let* ((*state* (to-array (aoc:lines input)))
         (*tracks* (copy-tracks *state*))
         (*carts* (collect-carts *state*)))
    (loop when (loop for cart in (sort-carts *carts*)
                     when (move (turn (corner cart))) return it)
            return it)))

(defun part2 (input)
  (let* ((*state* (to-array (aoc:lines input)))
         (*tracks* (copy-tracks *state*))
         (*carts* (collect-carts *state*)))
    (loop while (< 1 (count-if #'cart-live *carts*)) do
      (loop for cart in (sort-carts *carts*)
            when (cart-live cart)
              do (move (turn (corner cart)))))
    (let ((last-cart (find-if #'cart-live *carts*)))
      (list (cart-y last-cart) (cart-x last-cart)))))
