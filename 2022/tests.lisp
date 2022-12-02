;;;; tests.lisp

(in-package #:aoc2022.tests)

(deftest day01 ()
  (is (= 66487 (aoc2022.day01::part1 (input-for 2022 1))))
  (is (= 197301 (aoc2022.day01::part2 (input-for 2022 1)))))

(deftest day02 ()
  (is (= 12855 (aoc2022.day02::part1 (input-for 2022 2))))
  (is (= 13726 (aoc2022.day02::part2 (input-for 2022 2)))))
