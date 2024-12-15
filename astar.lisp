;;;; astar.lisp

(in-package :astar)

(defstruct (queue-entry :conc-name) index node)

(defstruct (search-node :conc-name) item fscore gscore previous)

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
    (or (compare (fscore x) (fscore y))
        (and compare-items
             (eql (fscore x) (fscore y))
             (funcall compare-items (item x) (item y))))))

(defun search (start neighbours goalp &key scoref distancef comparef max-distance)
  (let ((queue (make-queue comparef))
        (entries (make-hash-table :test 'equalp)))
    (flet ((%donep (node)
             (funcall goalp (item node)))
           (%distance (x y)
             (if distancef
                 (funcall distancef x y)
                 1))
           (%score (item)
             (if scoref
                 (funcall scoref item)
                 1))
           (get-entry (item &optional fscore gscore)
             (or (gethash item entries)
                 (setf (gethash item entries)
                       (make-queue-entry :node
                                         (make-search-node :item item
                                                           :fscore fscore
                                                           :gscore gscore))))))
      (q:queue-insert queue (get-entry start 0 (%score start) 0))
      (loop until (q:queue-empty-p queue)
            for entry = (q:queue-pop queue)
            for current = (node entry)
            when (%donep current)
              return current
            until (and max-distance (>= (gscore current) max-distance))
            do (loop for neighbour-item in (funcall neighbours (item current))
                     for neighbour-entry = (get-entry neighbour-item)
                     for neighbour-node = (node neighbour-entry)
                     for neighbour-score = (or (gscore neighbour-node)
                                               (setf (gscore neighbour-node)
                                                     (%score neighbour-item)))
                     for alt = (+ (gscore current) (%distance (item current) neighbour-item))
                     when (< alt neighbour-score)
                       do (setf (gscore neighbour-node) alt
                                (fscore neighbour-node) (+ alt neighbour-score)
                                (previous neighbour-node) current)
                          (requeue queue neighbour-entry))))))
