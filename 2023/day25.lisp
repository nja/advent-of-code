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
(defstruct (edge :conc-name (:constructor edge (eid a b)) (:print-function print-edge)) eid a b)

(defun make-edge (a b)
  (let ((nodes (sort (list a b) #'string< :key (a:compose #'symbol-name #'id))))
    (edge (format nil "~a+~a" (id (first nodes)) (id (second nodes)))
          (first nodes)
          (second nodes))))

(defun print-node (node stream depth)
  (declare (ignore depth))
  (format stream "<~a:~{~a~^,~}>" (id node) (mapcar #'id (connected node))))

(defun print-edge (edge stream depth)
  (declare (ignore depth))
  (format stream "~a:~a/~a" (eid edge) (id (a edge)) (id (b edge))))

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

(defun remove-edge (edge)
  (let ((a (a edge))
        (b (b edge)))
    (setf (edges a) (delete edge (edges a))
          (edges b) (delete edge (edges b))
          (a edge) nil
          (b edge) nil)))

;; (defun remove-self-edges (node)
;;   (setf))

(defun vector-remove (vector i)
  (assert (< i (fill-pointer vector)))
  (prog1 (aref vector i)
    (setf (aref vector i) (vector-pop vector))))

(defun kargers (edges nodes)
  (loop while (< 2 (hash-table-count nodes))
        for edge = (vector-remove edges (random (length edges)))
        for c = (contract edge)
        do (remhash (id (a edge)) nodes)
           (remhash (id (b edge)) nodes)
           (setf (gethash (id c) nodes) c)
           (loop for i from 0
                 while (< i (length edges))
                 for e = (aref edges i)
                 when (eq (a e) (b e))
                   do (vector-remove edges i)
                      (remove-edge e))
        finally (return (a:hash-table-values nodes))))
