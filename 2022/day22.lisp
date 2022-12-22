;;;; day22.lisp

(in-package :aoc2022.day22)

(defun parse (input)
  (multiple-value-bind (array side) (to-array (first (aoc:sections input)))
    (values array side (directions (second (aoc:sections input))))))

(defun to-array (section)
  (let ((side (reduce #'min (ppcre:all-matches-as-strings "[.#]+" section)
                      :key #'length))
        (faces (make-array 8)))
    (labels ((first-row-face ()
               (loop for i from 1
                     when (< (aref faces i) (* side side))
                       do (return i)))
             (face (first x)
               (+ first (truncate x side))))
      (loop
        with lines = (aoc:lines section)
        with rows = (+ 2 (length lines))
        and cols = (+ 2 (reduce #'max lines :key #'length))
        with array = (make-array (list rows cols) :initial-element #\Space)
        for line in lines
        for row from 1
        do (loop with first = (first-row-face)
                 for c across line
                 for col from 1
                 for face = (face first x)
                 count (not (eql c #\Space)) into x
                 unless (eql #\Space c)
                   do (incf (aref faces face))
                 do (setf (aref array row col)
                          (case c
                            ((#\# #\Space) c)
                            (#\. face))))
        finally (return (values array side))))))

(defun directions (string)
  (let ((spaced (ppcre:regex-replace-all "(R|L)" string " \\1 " ))
        (*package* (symbol-package 'directions)))
    (read-from-string (format nil "(~a)" spaced))))

(defun pos (x y) (list x y))
(defun x (pos) (first pos))
(defun y (pos) (second pos))

(defun fpos (pos)
  (pos (mod (1- (x pos)) *side*)
       (mod (1- (y pos)) *side*)))

(defun add (a b)
  (mapcar #'+ (hpos a) (hpos b)))

(defun inv (pos)
  (mapcar (a:curry #'* -1) pos))

(defparameter *headings*
  (a:circular-list '(> 0 (1 0)) '(v 1 (0 1)) '(< 2 (-1 0)) '(^ 3 (0 -1))))

(defun hpos (x)
  (if (integerp (first x))
      x
      (caddr x)))

(defun hscore (h)
  (second h))

(defun hsign (h)
  (first h))

(defun h (sign)
  (and (member sign '(> v < ^))
       (member sign *headings* :key #'hsign)))

(defun r (headings)
  (cdr headings))

(defun l (headings)
  (cdddr headings))

(defun posref (array pos)
  (aref array (second pos) (first pos)))

(defun take-step (array pos h)
  (let ((at (add pos (hpos h))))
    (ecase (posref array at)
      ((1 2 3 4 5 6) (values at h))
      (#\# (values pos h))
      (#\Space (wrap-around array pos h)))))

(defun wrap-around (array pos h)
  (let ((d (inv (hpos h))))
    (loop for p = pos then n
          for n = (add p d)
          for c = (posref array n)
          when (eql #\Space c)
            do (return (values (case (posref array p)
                                 ((1 2 3 4 5 6) p)
                                 (#\# pos))
                               h)))))

(defun walk (array directions)
  (loop with headings = *headings*
        with pos = (start-pos array)
        for x in directions
        when (symbolp x)
          do (setf headings (funcall x headings))
        when (integerp x)
          do (dotimes (i x)
               (multiple-value-bind (new-pos new-h) (take-step array pos (first headings))
                 (setf pos new-pos
                       headings (h (hsign new-h)))))
        finally (return (cons (hscore (first headings)) pos))))

(defun start-pos (array)
  (loop for row from 0 below (array-dimension array 0) do
    (loop for col from 0 below (array-dimension array 1)
          when (integerp (aref array row col))
            do (return-from start-pos (pos col row)))))

(defun password (facing col row)
  (+ (* 1000 row) (* 4 col) facing))

(defun part1 (input)
  (multiple-value-bind (array *side* directions) (parse input)
    (apply #'password (walk array directions))))

(defun cube-walk (array directions)
  (loop with headings = *headings*
        with pos = (start-pos array)
        for x in directions
        when (symbolp x)
          do (setf headings (funcall x headings))
        when (integerp x)
          do (dotimes (i x)
               (multiple-value-bind (new-pos new-h) (cube-step array pos (first headings))
                 (setf pos new-pos
                       headings (h (hsign new-h)))))
        finally (return (cons (hscore (first headings)) pos))))

(defun cube-step (array pos h)
  (let* ((face (posref array pos))
         (next-pos (add pos (hpos h)))
         (next (posref array next-pos)))
    (ecase next
      (#\# (values pos h))
      ((1 2 3 4 5 6 #\Space)
       (if (eql face next)
           (values next-pos h)
           (multiple-value-bind (next-pos next-h) (cube-around array pos h)
             (case (posref array next-pos)
               ((1 2 3 4 5 6) (values next-pos next-h))
               (t (values pos h)))))))))

(defun cube-around (array pos h)
  (let* ((key (list (posref array pos) (hsign h)))
         (item (find key *map* :test #'a:starts-with-subseq)))
    (destructuring-bind (face op) (cddr item)
      (let ((new-pos
              (array-pos array face
                         (funcall (case op
                                    (wrap #'wrap-pos)
                                    (clockwise #'clockwise-pos)
                                    (counter-clockwise #'counter-clockwise-pos)
                                    (invert #'invert-pos))
                                  (fpos pos)
                                  (hsign h))))
            (new-h
              (first (h (funcall (case op
                                   (wrap #'identity)
                                   (clockwise #'counter-clockwise-sign)
                                   (counter-clockwise #'clockwise-sign)
                                   (invert #'invert-sign))
                                 (hsign h))))))
        (values new-pos new-h)))))

(defun symmetric-getf (x plist)
  (or (getf plist x)
      (getf (reverse plist) x)))

(defun circular-getf (x plist)
  (or (getf plist x)
      (getf (cons (a:lastcar plist) plist) x)))

(defun wrap-pos (fpos sign)
  (mapcar (a:rcurry #'mod *side*) (add fpos (hpos (first (h sign))))))

(defun clockwise-pos (fpos sign)
  (declare (ignore sign))
  (pos (y fpos) (x fpos)))

(defun counter-clockwise-pos (fpos sign)
  (declare (ignore sign))
  (pos (y fpos) (x fpos)))

(defun invert-pos (fpos sign)
  (declare (ignore sign))
  (pos (x fpos) (- *side* (1+ (y fpos)))))

(defun invert-sign (sign)
  (symmetric-getf sign '(> < ^ v)))

(defun clockwise-sign (sign)
  (circular-getf sign '(> v < ^)))

(defun counter-clockwise-sign (sign)
 (clockwise-sign (invert-sign sign)))

(defun map-cube (list)
  (let (map)
    (dolist (item list map)
      (destructuring-bind (from sign to op) item
        (push item map)
        (push (list to
                    (funcall (case op
                               (wrap #'invert-sign)
                               (clockwise #'clockwise-sign)
                               (counter-clockwise #'counter-clockwise-sign)
                               (invert #'identity))
                             sign)                      
                    from
                    (or (symmetric-getf op '(clockwise counter-clockwise)) op))
              map)))))

(defparameter *map*
  (map-cube '((1 > 2 wrap) (1 v 3 wrap) (1 < 4 invert) (1 ^ 6 counter-clockwise)
              (3 > 2 clockwise) (2 ^ 6 wrap) (6 ^ 4 wrap) (4 ^ 3 counter-clockwise)
              (5 > 2 invert) (5 v 6 counter-clockwise) (5 < 4 wrap) (5 ^ 3 wrap))))

(defparameter *side* nil)

(defun array-pos (array face face-pos)
  (loop for i below (array-total-size array)
        when (eql face (row-major-aref array i))
          do (multiple-value-bind (row col) (truncate i (array-dimension array 1))
               (return (add (pos (1+ (* *side* (truncate col *side*)))
                                 (1+ (* *side* (truncate row *side*))))
                            face-pos)))))

(defun part2 (input)
  (multiple-value-bind (array *side* directions) (parse input)
    (apply #'password (cube-walk array directions))))
