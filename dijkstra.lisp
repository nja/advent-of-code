;;;; dijkstra.lisp

(in-package #:dijkstra)

(defstruct (queue-entry :conc-name) index node)

(defstruct (search-node :conc-name) item distance previous)

(defun make-queue (compare-items)
  (q:make-empty-queue
   (let ((node-comparer (node-comparer compare-items)))
     (lambda (entry-x entry-y)
       (funcall node-comparer (node entry-x) (node entry-y))))
   (lambda (x i) (setf (index x) i))
   #'index))

(defun compare (x y)
  (or (and x (null y))
      (and x y (< x y))))

(defun node-comparer (&optional compare-items)
  (lambda (x y)
    (or (compare (distance x) (distance y))
        (and compare-items
             (eql (distance x) (distance y))
             (funcall compare-items (item x) (item y))))))

(defun search* (start neighbours &key donep (goal nil goalp) distancef comparef max-distance)
  (let ((queue (make-queue comparef))
        (entries (make-hash-table :test 'equalp)))
    (flet ((%donep (node)
             (or (and goalp (equalp goal (item node)))
                 (and donep (funcall donep (item node)))))
           (%distance (x y)
             (if distancef
                 (funcall distancef x y)
                 1))
           (get-entry (item &optional distance)
             (or (gethash item entries)
                 (setf (gethash item entries)
                       (make-queue-entry :node
                                         (make-search-node :item item
                                                           :distance distance))))))
      (q:queue-insert queue (get-entry start 0))
      (or (loop until (q:queue-empty-p queue)
                for entry = (q:queue-pop queue)
                for current = (node entry)
                when (%donep current)
                  do (return current)
                until (and max-distance (>= (distance current) max-distance))
                do (loop for neighbour-item in (funcall neighbours (item current))
                         for neighbour-entry = (get-entry neighbour-item)
                         for neighbour-node = (node neighbour-entry)
                         for alt = (+ (distance current)
                                      (%distance (item current) (item neighbour-node)))
                         when (compare alt (distance neighbour-node))
                           do (setf (distance neighbour-node) alt
                                    (previous neighbour-node) current)
                              (requeue queue neighbour-entry)))
          (unless (or donep goalp)
            (mapcar #'node (a:hash-table-values entries)))))))

(defun requeue (queue entry)
  (if (and (index entry) (q:queue-find queue entry))
      (q:queue-update queue entry)
      (q:queue-insert queue entry)))
