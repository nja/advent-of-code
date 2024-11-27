;;;; day25.lisp

(in-package :aoc2023.day25)

(defvar *test*
"jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr")

(defparameter *test2*
"a b c d
b c d
c d")

(defparameter *test3*
"a b c d
b c d
c d
d e
e f g h i j k l m n o
f g h i j k l m n o
g h i j k l m n o
h i j k l m n o
i j k l m n o
j k l m n o
k l m n o
l m n o
m n o
n o")


(defun safe-parse (input)
  (let ((connections (mapcar (lambda (line) (read-from-string (format nil "(~a)" (aoc:tr ":" " " line))))
                             (aoc:lines input)))
        (nodes (make-hash-table)))
    (dolist (ids connections)
      (let ((nodes (mapcar (lambda (id) (or (gethash id nodes)
                                            (setf (gethash id nodes) (node id))))
                           ids)))
        (let ((a (first nodes)))
          (dolist (b (remove a (remove-duplicates (rest nodes))))
            (unless (find b (connected a))
              (let ((edge (make-edge a b)))
                (push edge (edges a))
                (push edge (edges b))))))))
    (let ((edges (remove-duplicates (a:flatten (mapcar #'edges (a:hash-table-values nodes))))))
      (values (make-array (length edges) :initial-contents edges :fill-pointer t)
              nodes))))


(defun parse (input)
  (let ((connections (mapcar (lambda (line) (read-from-string (format nil "(~a)" (aoc:tr ":" " " line))))
                             (aoc:lines input)))
        (nodes (make-hash-table))
        edges)
    (dolist (ids connections)
      (let ((nodes (mapcar (lambda (id) (or (gethash id nodes)
                                            (setf (gethash id nodes) (node id))))
                           ids)))
        (let ((a (first nodes)))
          (dolist (b (rest nodes))
            (let ((edge (make-edge a b)))
              (push edge edges)
              (push edge (edges a))
              (push edge (edges b)))))))
    (values (make-array (length edges) :initial-contents edges :fill-pointer t)
            nodes)))

(defstruct (node :conc-name (:constructor node (id &optional (size 1))) (:print-function print-node)) id edges size)
(defstruct (edge :conc-name (:constructor edge (a b)) (:print-function print-edge)) a b)

(defun make-edge (a b)
  (if (string< (symbol-name (id a)) (symbol-name (id b)))
      (edge a b)
      (edge b a)))

(defun abbrev (s)
  (cond ((symbolp s) (abbrev (symbol-name s)))
        ((< (length s) 30) s)
        (t (format nil "~a..." (subseq s 0 27)))))

(defun print-node (node stream depth)
  (declare (ignore depth))
  (format stream "<~a:~{~a~^,~}>[~d]"
          (abbrev (id node))
          (let (connected)
            (maphash (lambda (n c)
                       (push (if (= 1 c)
                                 (abbrev (id n))
                                 (format nil "~a(~d)" (abbrev (id n)) c))
                             connected))
                     (node-summary (connected node)))
            connected)
          (size node)))

(defun node-summary (nodes)
  (let ((counts (make-hash-table)))
    (dolist (n nodes counts)
      (incf (gethash n counts 0)))))

;; (defun pair (a b)
;;   (if (string< (symbol-name a) (symbol-name b))
;;       (cons a b)
;;       (cons b a)))

(defun print-edge (edge stream depth)
  (declare (ignore depth))
  (format stream "~a/~a" (abbrev (id (a edge))) (abbrev (id (b edge)))))

(defun connected (node)
  (mapcan (lambda (edge) (remove node (list (a edge) (b edge))))
          (edges node)))

(defun contract (edge)
  (let ((a (a edge))
        (b (b edge)))
    (let ((c (node (combine-symbols (id a) (id b)) (+ (size a) (size b)))))
      (setf (edges c) (nunion (mapcar (a:curry #'replace-node c a) (edges a))
                              (mapcar (a:curry #'replace-node c b) (edges b))))
      c)))

(defun combine-symbols (a b)
  (intern (format nil "~a+~a" a b)
          (symbol-package a)))

(defun replace-node (new old edge)
  (when (eq old (a edge))
    (setf (a edge) new))
  (when (eq old (b edge))
    (setf (b edge) new))
  edge)

(defun remove-self-edges (node)
  (setf (edges node) (delete-if (lambda (edge)
                                  (and (eq node (a edge))
                                       (eq node (b edge))))
                                (edges node))))

(defun prune-edges (vector)
  (delete-if (lambda (edge) (eq (a edge) (b edge))) vector))

(defun vector-remove (vector i)
  (assert (< i (fill-pointer vector)))
  (prog1 (aref vector i)
    (setf (aref vector i) (vector-pop vector))))

(defun kargers (edges nodes)
  (loop while (< 2 (hash-table-count nodes))
        for edge = (vector-remove edges (random (length edges)))
        for a = (a edge)
        for b = (b edge)
        for c = (contract edge)
        do (remhash (id a) nodes)
           (remhash (id b) nodes)
           (setf (gethash (id c) nodes) c)
           (prune-edges edges)
        finally (return (values (a:hash-table-values nodes)
                                edges))))

(defun sample (input n promise)
  (loop for nodes = (nth-value 0 (multiple-value-call #'kargers (parse input)))
        when (eql n (length (edges (first nodes))))
          do (lparallel:fulfill promise (reduce #'* nodes :key #'size))
        until (lparallel:fulfilledp promise)))

;; (defun sample (input)
;;   (multiple-value-bind (nodes edges) (multiple-value-call #'kargers (parse input))
;;     (declare (ignore edges))
;;     (assert (= 2 (length nodes)))
;;     (reduce #'* nodes :key #'size)))

(defun psample (input n)
  (let ((inputs (make-array n :initial-element input))
        (promise (lparallel:promise)))
    (lparallel:pmap 'vector (lambda (x) (lparallel:future (sample x n promise))) inputs)
    promise))

(defun stats (samples)
  (let ((stats (make-hash-table)))
    (map nil (lambda (x) (incf (gethash x stats 0))) samples)
    (sort (a:hash-table-alist stats) #'> :key #'cdr)))

(defun hmm (input)
  (loop for nodes = (nth-value 0 (multiple-value-call #'kargers (parse input)))
        until (= 525264 (reduce #'* nodes :key #'size))
        finally (return nodes)))

(defun hmmn (input n)
  (loop for nodes = (nth-value 0 (multiple-value-call #'kargers (parse input)))
        until (eql n (length (edges (first nodes))))
        finally (return nodes)))