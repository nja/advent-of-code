;;;; day24.lisp

(in-package #:aoc2018.day24)

(defun parse-armies (input)
  (mapcan (lambda (section)
            (let ((army (find-if (lambda (s)
                                   (str:starts-with? (string s) section :ignore-case t))
                                 '(immune infection))))
              (loop for line in (rest (aoc:lines section))
                    for i from 1
                    for group = (parse-group army line)
                    do (setf (nr group) i)
                    collect group)))
          (aoc:sections input)))

(defun parse-group (army line)
  (destructuring-bind (units hp damage initiative) (clean line)
    (make-group :army army :units units :hp hp :attack-damage damage :initiative initiative
                :immunities (parse-attack-types "immune to [^;)]+" line)
                :weaknesses (parse-attack-types "weak to [^;)]+" line)
                :attack-type (first (parse-attack-types "\\w+ damage at" line)))))

(defun parse-attack-types (regex line)
  (a:when-let ((match (ppcre:scan-to-strings regex line)))
    (remove-if-not (lambda (s) (str:contains? (string s) match :ignore-case t))
                   '(slashing radiation bludgeoning fire cold))))

(defun clean (line)
  (mapcar #'parse-integer (remove-if (a:curry #'string= "") (ppcre:split "[^0-9-]+" line))))

(defstruct (group :conc-name)
  army nr units hp initiative
  attack-type attack-damage
  weaknesses immunities)

(defun fight (groups)
  (loop for (attacker target) in (sort (target-selection groups)
                                         #'attack-order-compare :key #'car)
        do (attack attacker target)
        finally (return (remove-if-not #'alive? groups))))

(defun target-selection (groups)
  (loop with targets
        with attackers = (sort (copy-list groups) #'target-selection-order-compare)
        with defenders = (copy-list groups)
        for attacker in attackers
        for target = (pick (target-preference-comparer attacker)
                           (enemies attacker defenders))
        when (and target (plusp (damage-dealt attacker target)))
          do (push (list attacker target) targets)
             (setf defenders (delete target defenders))
        finally (return targets)))

(defun target-selection-order-compare (a b)
  (aoc:comparisons a b (>) effective-power initiative))

(defun target-preference-comparer (attacker)
  (flet ((damage (x) (damage-dealt attacker x)))
    (lambda (a b)
      (aoc:comparisons a b (>) damage effective-power initiative))))

(defun damage-dealt (attacker target)
  (flet ((is? (f) (find (attack-type attacker) (funcall f target))))
    (* (units attacker) (attack-damage attacker)
       (cond ((is? #'immunities) 0)
             ((is? #'weaknesses) 2)
             (t 1)))))

(defun effective-power (group)
  (* (units group) (attack-damage group)))

(defun attack-order-compare (a b)
  (> (initiative a) (initiative b)))

(defun attack (attacker target)
  (when (alive? attacker)
      (let ((damage (damage-dealt attacker target)))
        (decf (units target)
              (units-lost (damage-dealt attacker target) (hp target)))
        damage)))

(defun alive? (group)
  (plusp (units group)))

(defun enemies (attacker groups)
  (remove (army attacker) groups :key #'army))

(defun pick (compare list)
  (loop with pick = (first list)
        for x in (rest list)
        do (setf pick (if (funcall compare x pick) x pick))
        finally (return pick)))

(defun units-lost (damage-dealt hp)
  (nth-value 0 (truncate damage-dealt hp)))

(defun winner? (groups)
  (when (every (a:compose (a:curry #'eq (army (first groups))) #'army) groups)
    (army (first groups))))

(defun war (groups)
  (loop for last-count = count
        for count = (reduce #'+ (mapcar #'units groups))
        until (or (eql count last-count) (winner? groups))
        do (setf groups (fight groups))
        finally (return (values count (winner? groups)))))

(defun part1 (input)
  (war (parse-armies input)))

(defun boost (groups army attack-boost)
  (mapcar (lambda (x)
            (let ((copy (copy-group x)))
              (when (eq army (army copy))
                (incf (attack-damage copy) attack-boost))
              copy))
          groups))

(defun fork (predicate &optional (lo 1) hi)
  (cond ((eq lo hi) (when (funcall predicate lo) lo))
        ((and hi (eq lo (1- hi))) (cond ((funcall predicate lo) lo)
                                        ((funcall predicate hi) hi)))
        (t (let ((mid (if hi
                          (truncate (+ lo hi) 2)
                          (* lo 2))))
             (if (funcall predicate mid)
                 (fork predicate lo mid)
                 (fork predicate mid hi))))))

(defun part2 (input)
  (let ((groups (parse-armies input)))
    (flet ((war* (boost) (war (boost groups 'immune boost))))
      (war* (fork (lambda (x) (eq 'immune (nth-value 1 (war* x)))))))))
