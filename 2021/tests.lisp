;;;; tests.lisp

(in-package #:aoc2021.tests)

(deftest day01 ()
  (is (= 1374 (aoc2021.day01::part1 (input-for 2021 1))))
  (is (= 1418 (aoc2021.day01::part2 (input-for 2021 1)))))

(deftest day02 ()
  (is (= 1882980 (aoc2021.day02::part1 (input-for 2021 2))))
  (is (= 1971232560 (aoc2021.day02::part2 (input-for 2021 2)))))