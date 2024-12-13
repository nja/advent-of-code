;;;; advent-of-code.asd
(asdf/parse-defsystem:defsystem :advent-of-code
  :description
  "Advent of Code"
  :author
  "Johan Andersson <nilsjohanandersson@gmail.com>"
  :license
  "MIT"
  :components
  ((:file "package") (:file "aoc") (:file "dijkstra") (:file "lalg")
   (:file "leaderboard") (:file "parse") (:file "setup") (:file "web")
   (:module "2015" :components
    ((:file "day01") (:file "day02") (:file "day03") (:file "day04")
     (:file "day05") (:file "day06") (:file "day07") (:file "day08")
     (:file "day09") (:file "day10") (:file "day11") (:file "day12")
     (:file "day13") (:file "day14") (:file "day15") (:file "day16")
     (:file "day17") (:file "day18") (:file "day19") (:file "day20")
     (:file "day21") (:file "day22") (:file "day23") (:file "day24")
     (:file "day25") (:file "tests")))
   (:module "2016" :components
    ((:file "day01") (:file "day02") (:file "day03") (:file "day04")
     (:file "day05") (:file "day06") (:file "day07") (:file "day08")
     (:file "day09") (:file "day10") (:file "day11") (:file "day12")
     (:file "day13") (:file "day14") (:file "day15") (:file "day16")
     (:file "day17") (:file "day18") (:file "day19") (:file "day20")
     (:file "day21") (:file "day22") (:file "day23") (:file "day24")
     (:file "day25") (:file "tests")))
   (:module "2017" :components
    ((:file "day01") (:file "day02") (:file "day03") (:file "day04")
     (:file "day05") (:file "day06") (:file "day07") (:file "day08")
     (:file "day09") (:file "day10") (:file "day11") (:file "day12")
     (:file "day13") (:file "day14") (:file "day15") (:file "day15.alt")
     (:file "day16") (:file "day17") (:file "day18") (:file "day19")
     (:file "day20") (:file "day21") (:file "day22") (:file "day23")
     (:file "day24") (:file "day25") (:file "tests")))
   (:module "2018" :components
    ((:file "day01") (:file "day02") (:file "day03") (:file "day04")
     (:file "day05") (:file "day06") (:file "day07") (:file "day08")
     (:file "day09") (:file "day10") (:file "day11") (:file "day12")
     (:file "day13") (:file "day14") (:file "day15") (:file "day16")
     (:file "day17") (:file "day18") (:file "day19") (:file "day20")
     (:file "day21") (:file "day22") (:file "day23") (:file "day24")
     (:file "day25") (:file "tests")))
   (:module "2019" :components
    ((:file "day01") (:file "day02") (:file "day03") (:file "day04")
     (:file "day05") (:file "day06") (:file "day07") (:file "day08")
     (:file "day09") (:file "day10") (:file "day11") (:file "day12")
     (:file "day13") (:file "day14") (:file "day15") (:file "day16")
     (:file "day17") (:file "tests")))
   (:module "2020" :components
    ((:file "day01") (:file "day02") (:file "day03") (:file "day04")
     (:file "day05") (:file "day06") (:file "day07") (:file "day08")
     (:file "day09") (:file "day10") (:file "day11") (:file "day12")
     (:file "day13") (:file "day14") (:file "day15") (:file "day16")
     (:file "day17") (:file "day18") (:file "day19") (:file "day20")
     (:file "day21") (:file "day22") (:file "day23") (:file "day24")
     (:file "day25") (:file "tests")))
   (:module "2021" :components
    ((:file "day01") (:file "day02") (:file "day03") (:file "day04")
     (:file "day05") (:file "day06") (:file "day07") (:file "day08")
     (:file "day09") (:file "day10") (:file "day11") (:file "day12")
     (:file "day13") (:file "day14") (:file "day15") (:file "day16")
     (:file "day17") (:file "day18") (:file "day19") (:file "day20")
     (:file "day21") (:file "day22") (:file "day23") (:file "day24")
     (:file "day25") (:file "tests")))
   (:module "2022" :components
    ((:file "day01") (:file "day02") (:file "day03") (:file "day04")
     (:file "day05") (:file "day06") (:file "day07") (:file "day08")
     (:file "day09") (:file "day10") (:file "day11") (:file "day12")
     (:file "day13") (:file "day14") (:file "day15") (:file "day16")
     (:file "day17") (:file "day18") (:file "day19") (:file "day20")
     (:file "day21") (:file "day22") (:file "day23") (:file "day24")
     (:file "day25") (:file "tests")))
   (:module "2023" :components
    ((:file "day01") (:file "day02") (:file "day03") (:file "day04")
     (:file "day05") (:file "day06") (:file "day07") (:file "day08")
     (:file "day09") (:file "day10") (:file "day11") (:file "day12")
     (:file "day13") (:file "day14") (:file "day15") (:file "day16")
     (:file "day17") (:file "day18") (:file "day19") (:file "day20")
     (:file "day21") (:file "day22") (:file "day23") (:file "day24")
     (:file "day25") (:file "tests")))
   (:module "2024" :components
    ((:file "day01") (:file "day02") (:file "day03") (:file "day04")
     (:file "day05") (:file "day06") (:file "day07") (:file "day08")
     (:file "day09") (:file "day10") (:file "day11") (:file "day12")
     (:file "day13") (:file "tests"))))
  :depends-on
  (#:alexandria #:arrow-macros #:cl-ppcre #:drakma #:fare-memoization #:fiasco
   #:fset #:ironclad #:jsown #:pettomato-indexed-priority-queue
   #:queues.simple-queue #:queues.priority-queue #:str #:lparallel))
