# Advent of Code

My solutions to [Advent of Code](https://adventofcode.com/)
using Common Lisp.

# Setup

Install [a Common Lisp
implementation](https://common-lisp.net/implementations) and
[QuickLisp](https://www.quicklisp.org/). I have only tested with
SBCL. At the very least, your implementation must support package-local nicknames.

# Demo

```
CL-USER> (push "~/src/advent-of-code/" ql:*local-project-directories*)
("~/src/advent-of-code/")
CL-USER> (ql:quickload "advent-of-code")
To load "advent-of-code":
  Load 1 ASDF system:
    advent-of-code
; Loading "advent-of-code"
..................................................
..................................................
..........................................
("advent-of-code")
CL-USER> (fiasco:run-tests 'aoc2022.tests)
AOC2022.TESTS (Suite)
  DAY01...................................................................[ OK ]
  DAY02...................................................................[ OK ]
  DAY03...................................................................[ OK ]
  DAY04...................................................................[ OK ]
  DAY05...................................................................[ OK ]
  DAY06...................................................................[ OK ]
  DAY07...................................................................[ OK ]
  DAY08...................................................................[ OK ]
  DAY09...................................................................[ OK ]

T
(#<test-run of AOC2022.TESTS: 10 tests, 18 assertions, 0 failures in 0.125 sec>)
CL-USER>
```
