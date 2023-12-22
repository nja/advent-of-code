;;;; day22.lisp

(in-package :aoc2023.day22)

(defparameter *test*
"1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9")

(defun parse (input)
  (mapcar (lambda (line)
            (mapcar (lambda (s) (mapcar #'parse-integer (str:split "," s)))
                    (str:split "~" line)))
          (aoc:lines input)))

(defun x (thing)
  (if (atom (car thing))
      (first thing)
      (mapcar #'x thing)))

(defun y (thing)
  (if (atom (car thing))
      (second thing)
      (mapcar #'y thing)))

(defun z (thing)
  (if (atom (car thing))
      (third thing)
      (mapcar #'z thing)))

(defun min-x (brick) (min (first (first brick)) (first (second brick))))
(defun min-y (brick) (min (second (first brick)) (second (second brick))))
(defun min-z (brick) (min (third (first brick)) (third (second brick))))

(defun max-x (brick) (max (first (first brick)) (first (second brick))))
(defun max-y (brick) (max (second (first brick)) (second (second brick))))
(defun max-z (brick) (max (third (first brick)) (third (second brick))))

(defun tallness (brick)
  (abs (apply #'- (z brick))))

(defun sort-by-z (bricks)
  (sort (copy-list bricks) #'< :key #'min-z))

(defun fall (bricks)
  (labels ((rec (bricks at-rest)
             (if (null bricks) at-rest
                 (let* ((new-height (resting-height (car bricks)))
                        (new-z (+ new-height (tallness (car bricks)))))
                   (map-horizontal (lambda (x y) (set-maxz x y new-z)) (car bricks))
                   (rec (cdr bricks) (cons (at-height (car bricks) new-height) at-rest))))))
    (nreverse (rec (sort-by-z bricks) nil))))

(defparameter *maxz* (make-hash-table :test 'equal))

(defun maxz (x y) (gethash (cons x y) *maxz* 0))
(defun set-maxz (x y z) (setf (gethash (cons x y) *maxz*) z))

(defun map-horizontal (f brick)
  (destructuring-bind ((xa ya za) (xb yb zb)) brick
      (declare (ignore za zb))
      (loop for x from (min xa xb) to (max xa xb) do
        (loop for y from (min ya yb) to (max ya yb)
              do (funcall f x y)))))

(defun resting-height (brick)
  (let ((height 0))
    (map-horizontal (lambda (x y) (setf height (max height (maxz x y))))
                    brick)
    (1+ height)))

(defun at-height (brick height)
  (destructuring-bind ((xa ya za) (xb yb zb)) brick
    (list (list xa ya height)
          (list xb yb (+ height (abs (- za zb)))))))

(defun hash-by-resting-height (bricks)
  (let ((hash (make-hash-table)))
    (dolist (brick bricks hash)
      (push brick (gethash (min-z brick) hash)))))

(defun hash-by-top (bricks)
  (let ((hash (make-hash-table)))
    (dolist (brick bricks hash)
      (push brick (gethash (max-z brick) hash)))))

(defun could-singly-be-disintegrated-predicate (bricks)
  (let ((by-top (hash-by-top bricks))
        (by-resting-height (hash-by-resting-height bricks)))
    (lambda (brick)
      (let ((resting-on-brick (remove-if-not (a:curry #'intersects-horizontally? brick)
                                             (gethash (1+ (max-z brick)) by-resting-height)))
            (possible-supports (remove brick (gethash (max-z brick) by-top))))
        ;; (terpri)
        ;; (print brick)
        ;; (print resting-on-brick)
        ;; (print possible-supports)
        (every (lambda (resting) (some (a:curry #'intersects-horizontally? resting)
                                       possible-supports))
               resting-on-brick)))))

(defun part1 (input)
  (let* ((*maxz* (make-hash-table :test 'equal))
         (bricks (fall (parse input))))
    (let ((answer (count-if (could-singly-be-disintegrated-predicate bricks) bricks)))
      (print (cond ((<= 431 answer) 'too-high)
                   (t 'in-range)))
      answer)))

(defun intersects-horizontally? (a b)
  (not (or (< (max-x a) (min-x b))
           (< (max-x b) (min-x a))
           (< (max-y a) (min-y b))
           (< (max-y b) (min-y a)))))

;; 2023 22 1: '431'
;; That's not the right answer; your answer is too high.
;; Please wait one minute before trying again.
;; (Cached until 2023-12-22 08:27:46)
;; 431
;; 2023 22 1: '428'
;; That's the right answer!
;; (Cached until 2023-12-22 08:30:57)

(defun part2 (input)
  (let* ((*maxz* (make-hash-table :test 'equal))
         (bricks (fall (parse input)))
         (unsafe (remove-if (could-singly-be-disintegrated-predicate bricks) bricks)))
    ))