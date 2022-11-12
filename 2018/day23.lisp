;;;; day23.lisp

(in-package #:aoc2018.day23)

(defun clean (line)
  (mapcar #'parse-integer (remove-if (a:curry #'string= "") (ppcre:split "[^0-9-]+" line))))

(defstruct (bot :conc-name (:constructor bot (x y z r))) x y z r)

(defun parse (input)
  (mapcar (a:compose (a:curry #'apply #'bot) #'clean) (aoc:lines input)))

(defun manhattan (a b)
  (+ (abs (- (x a) (x b)))
     (abs (- (y a) (y b)))
     (abs (- (z a) (z b)))))

(defun strongest (bots)
  (first (sort (copy-seq bots) #'> :key #'r)))

(defun in-range? (this that)
  (< (manhattan this that) (r this)))

(defun part1 (input)
  (let ((bots (parse input)))
    (length (remove-if-not (a:curry #'in-range? (strongest bots))
                           bots))))

(defparameter *origin* (bot 0 0 0 0))

(defun intersects? (r b)
  (flet ((dist (dim)
           (max 0 (- (abs (- (funcall dim r) (funcall dim b))) (r r)))))
    (<= (+ (dist #'x) (dist #'y) (dist #'z))
        (r b))))

(defun intersection-count (bots x)
  (count-if (a:curry #'intersects? x) bots))

(defun first-region (bots)
  (let ((max-manhattan (reduce #'max bots :key (a:curry #'manhattan *origin*))))
    (bot 0 0 0 (expt 2 (ceiling (log (/ max-manhattan 3) 2))))))

(defun split (region)
  (loop with p = (shrink region)
        for d in (corners (r p))
        collect (add p d)))

(defun add (p d)
  (bot (+ (x p) (x d))
       (+ (y p) (y d))
       (+ (z p) (z d))
       (r p)))

(defun shrink (region)
  (bot (x region) (y region) (z region)
       (if (eq 1 (r region))
           0
           (ceiling (r region) 2))))

(defun corners (r)
  (loop for x in '(1 -1) nconc
    (loop for y in '(1 -1) nconc
      (loop for z in '(1 -1)
            collect (bot (* x r) (* y r) (* z r) 0)))))

(defun divide (region bots)
  (loop for regions = (split region) then (mapcan #'split best)
        for best = (top (a:curry #'intersection-count bots) regions)
        while (some (lambda (r) (> (r r) 0)) best)
        finally (return best)))

(defun top (scoref list)
  (let* ((scored (mapcar (lambda (r) (cons (funcall scoref r) r)) list))
         (sorted (sort scored #'> :key #'car))
         (top-score (caar sorted)))
    (mapcar #'cdr (remove-if (a:curry #'> top-score) sorted :key #'car))))

(defun part2 (input)
  (let* ((bots (parse input)))
    (reduce #'min (divide (first-region bots) bots) :key (a:curry #'manhattan *origin*))))
