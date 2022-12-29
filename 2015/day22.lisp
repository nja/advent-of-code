;;;; day22.lisp

(in-package :aoc2015.day22)

(defstruct (effect :conc-name)
  name cost
  (instant-damage 0)
  (instant-hp 0)
  (armor 0)
  (damage 0)
  (mana 0)
  (turns 0))

(defparameter *spells*
  (reverse (list (make-effect :name 'magic-missile :cost 53 :instant-damage 4)
                 (make-effect :name 'drain :cost 73 :instant-damage 2 :instant-hp 2)
                 (make-effect :name 'shield :cost 113 :armor 7 :turns 6)
                 (make-effect :name 'poison :cost 173 :damage 3 :turns 6)
                 (make-effect :name 'recharge :cost 229 :mana 101 :turns 5))))

(defun boss (input)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" input)))

(defun age (effects)
  (labels ((age (effect)
             (let ((copy (copy-effect effect)))
               (when (plusp (decf (turns copy)))
                 copy))))
    (remove nil (mapcar #'age effects))))

(defun after-effects (f hp mana spent boss-hp effects next)
  (funcall f
           hp
           (+ mana (sum effects #'mana))
           spent
           (- boss-hp (sum effects #'damage))
           (age effects)
           next))

(defun sum (effects key)
  (reduce #'+ effects :key key))

(defun castable (mana effects spells)
  (remove-if (lambda (spell)
               (or (< mana (cost spell))
                   (find (name spell) effects :key #'name)))
             spells))

(defparameter *difficulty* 0)

(defun fight (boss-hp boss-damage)
  (let (best)
    (labels ((win (spent next)
               (setf best (min spent (or best spent)))
               (funcall next))
             (player (hp mana spent boss-hp effects next)
               (decf hp *difficulty*)
               (cond ((not (plusp hp))
                      (funcall next))
                     (t (labels ((next-spell (spells)
                                   (if spells
                                       (let ((spell (car spells)))
                                         (after-effects #'boss
                                                        (+ hp (instant-hp spell))
                                                        (- mana (cost spell))
                                                        (+ spent (cost spell))
                                                        (- boss-hp (instant-damage spell))
                                                        (cons spell effects)
                                                        (lambda () (next-spell (cdr spells)))))
                                       (funcall next))))
                          (next-spell (castable mana effects *spells*))))))
             (boss (hp mana spent boss-hp effects next)
               (cond ((not (plusp boss-hp))
                      (win spent next))
                     ((and best (<= best spent))
                      (funcall next))
                     ((not (plusp (decf hp (damage-done boss-damage (sum effects #'armor)))))
                      (funcall next))
                     (t
                      (after-effects #'player hp mana spent boss-hp effects next)))))
      (player 50 500 0 boss-hp nil (lambda () best)))))

(defun damage-done (damage armor)
  (max 1 (- damage armor)))

(defun part1 (input)
  (apply #'fight (boss input)))

(defun part2 (input)
  (let ((*difficulty* 1))
    (apply #'fight (boss input))))
