module Solutions (ProblemRunner, solutions) where

import qualified Data.Map.Strict as M
import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day2
import qualified Day20
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9

type ProblemRunner = FilePath -> IO ()

solutions :: M.Map Integer (ProblemRunner, ProblemRunner)
solutions =
  M.fromList
    [ (1, (Day1.part1, Day1.part2)),
      (2, (Day2.part1, Day2.part2)),
      (3, (Day3.part1, Day3.part2)),
      (4, (Day4.part1, Day4.part2)),
      (5, (Day5.part1, Day5.part2)),
      (6, (Day6.part1, Day6.part2)),
      (7, (Day7.part1, Day7.part2)),
      (8, (Day8.part1, Day8.part2)),
      (9, (Day9.part1, Day9.part2)),
      (10, (Day10.part1, Day10.part2)),
      (11, (Day11.part1, Day11.part2)),
      (12, (Day12.part1, Day12.part2)),
      (13, (Day13.part1, Day13.part2)),
      (14, (Day14.part1, Day14.part2)),
      (15, (Day15.part1, Day15.part2)),
      (16, (Day16.part1, Day16.part2)),
      (17, (Day17.part1, Day17.part2)),
      (18, (Day18.part1, Day18.part2)),
      (19, (Day19.part1, Day19.part2)),
      (20, (Day20.part1, Day20.part2))
    ]
