;;;; tests.lisp

(in-package #:aoc2022.tests)

(deftest day01 ()
  (is (= 66487 (aoc2022.day01::part1 (input-for 2022 1))))
  (is (= 197301 (aoc2022.day01::part2 (input-for 2022 1)))))
