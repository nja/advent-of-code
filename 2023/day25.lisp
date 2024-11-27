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

(defvar *test2*
"a b c d
b c d
c d")

(defun parse (input)
  (let ((connections (mapcar (lambda (line) (read-from-string (format nil "(~a)" (aoc:tr ":" " " line))))
                             (aoc:lines input)))
        (nodes (make-hash-table)))
    (dolist (ids connections)
      (let ((nodes (mapcar (lambda (id) (or (gethash id nodes)
                                            (setf (gethash id nodes) (node id))))
                           ids)))
        (let ((a (first nodes)))
          (dolist (b (rest nodes))
            (let ((edge (make-edge a b)))
              (push edge (edges a))
              (push edge (edges b)))))))
    (let ((edges (remove-duplicates (a:flatten (mapcar #'edges (a:hash-table-values nodes))))))
      (values (make-array (length edges) :initial-contents edges :fill-pointer t)
              nodes))))

(defstruct (node :conc-name (:constructor node (id &optional (size 1))) (:print-function print-node)) id edges size)
(defstruct (edge :conc-name (:constructor edge (a b)) (:print-function print-edge)) a b)

(defun make-edge (a b)
  (if (string< (symbol-name (id a)) (symbol-name (id b)))
      (edge a b)
      (edge b a)))

(defun print-node (node stream depth)
  (declare (ignore depth))
  (format stream "<~a:~{~a~^,~}>[~d]"
          (id node)
          (let (connected)
            (maphash (lambda (n c)
                       (push (if (= 1 c)
                                 (id n)
                                 (format nil "~a(~d)" (id n) c))
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
  (format stream "~a/~a" (id (a edge)) (id (b edge))))

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

(defun combine-symbols (&rest symbols)
  (let ((names (sort (mapcar #'symbol-name symbols) #'string<)))
   (intern (format nil "~{~a~^+~}" names) (symbol-package (first symbols)))))

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
