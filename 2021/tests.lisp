;;;; tests.lisp

(in-package #:aoc2021.tests)

(deftest day01 ()
  (is (= 1374 (aoc2021.day01::part1 (input-for 2021 1))))
  (is (= 1418 (aoc2021.day01::part2 (input-for 2021 1)))))
