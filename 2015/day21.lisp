;;;; day21.lisp

(in-package :aoc2015.day21)

(defstruct (thing :conc-name) name (cost 0) (damage 0) (armor 0) (hp 0))

(defparameter *shop* (remove #\Return
"Weapons:    Cost  Damage  Armor
Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0

Armor:      Cost  Damage  Armor
Birthday suit 0     0       0
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5

Rings:      Cost  Damage  Armor
Wedding band  0     0       0
Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3"))

(defun shop-items ()
  (flet ((items (section)
           (mapcar (lambda (line)
                     (ppcre:register-groups-bind (name (#'parse-integer a b c))
                         ("(.+\\S) +(\\d+) +(\\d+) +(\\d+)" line)
                       (make-thing :name name :cost a :damage b :armor c)))
                   (cdr (aoc:lines section)))))
    (mapcar #'items (aoc:sections *shop*))))

(defun loadouts (cost-predicate)
  (destructuring-bind (weapons armors rings) (shop-items)
    (let (result (player (make-thing :name "Player" :hp 100)))
      (a:map-product
       (lambda (weapon armor left-ring right-ring)
         (when (not (eq left-ring right-ring))
           (push (list player weapon armor left-ring right-ring) result)))
       weapons armors rings rings)
      (sort (remove nil result)
            cost-predicate :key (a:rcurry #'sum #'cost)))))

(defun boss (input)
  (destructuring-bind (hp damage armor)
      (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" input))
    (make-thing :name "Boss" :hp hp :damage damage :armor armor)))

(defun sum (loadout key)
  (reduce #'+ loadout :key key))

(defun fight (boss loadout)
  (loop with hp = (sum loadout #'hp)
        with damage = (sum loadout #'damage)
        with armor = (sum loadout #'armor)
        with hp-boss = (hp boss)
        do (decf hp-boss (max 1 (- damage (armor boss))))
        unless (plusp hp-boss)
          return loadout
        do (decf hp (max 1 (- (damage boss) armor)))
        unless (plusp hp)
          return nil))

(defun part1 (input)
  (sum (find-if (a:curry #'fight (boss input)) (loadouts #'<))
       #'cost))

(defun part2 (input)
  (sum (find-if-not (a:curry #'fight (boss input)) (loadouts #'>))
       #'cost))
