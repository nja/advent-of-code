;;;; tests.lisp

(in-package #:aoc2021.tests)

(deftest day01 ()
  (is (= 1374 (aoc2021.day01::part1 (input-for 2021 1))))
  (is (= 1418 (aoc2021.day01::part2 (input-for 2021 1)))))

(deftest day02 ()
  (is (= 1882980 (aoc2021.day02::part1 (input-for 2021 2))))
  (is (= 1971232560 (aoc2021.day02::part2 (input-for 2021 2)))))

(deftest day03 ()
  (is (= 3923414 (aoc2021.day03::part1 (input-for 2021 3))))
  (is (= 5852595 (aoc2021.day03::part2 (input-for 2021 3)))))

(deftest day04 ()
  (is (= 25410 (aoc2021.day04::part1 (input-for 2021 4))))
  (is (= 2730 (aoc2021.day04::part2 (input-for 2021 4)))))

(deftest day05 ()
  (is (= 6710 (aoc2021.day05::part1 (input-for 2021 5))))
  (is (= 20121 (aoc2021.day05::part2 (input-for 2021 5)))))
