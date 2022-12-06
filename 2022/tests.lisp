;;;; tests.lisp

(in-package #:aoc2022.tests)

(deftest day01 ()
  (is (= 66487 (aoc2022.day01::part1 (input-for 2022 1))))
  (is (= 197301 (aoc2022.day01::part2 (input-for 2022 1)))))

(deftest day02 ()
  (is (= 12855 (aoc2022.day02::part1 (input-for 2022 2))))
  (is (= 13726 (aoc2022.day02::part2 (input-for 2022 2)))))

(deftest day03 ()
  (is (= 7875 (aoc2022.day03::part1 (input-for 2022 3))))
  (is (= 2479 (aoc2022.day03::part2 (input-for 2022 3)))))

(deftest day04 ()
  (is (= 526 (aoc2022.day04::part1 (input-for 2022 4))))
  (is (= 886 (aoc2022.day04::part2 (input-for 2022 4)))))

(deftest day05 ()
  (is (string= "NTWZZWHFV" (aoc2022.day05::part1 (aoc:input-for 2022 5))))
  (is (string= "BRZGFVBTJ" (aoc2022.day05::part2 (aoc:input-for 2022 5)))))

(deftest day06 ()
  (is (= 1640 (aoc2022.day06::part1 (aoc:input-for 2022 6))))
  (is (= 3613 (aoc2022.day06::part2 (aoc:input-for 2022 6)))))
