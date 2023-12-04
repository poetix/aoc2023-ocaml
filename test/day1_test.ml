open Aoc2023.Day1

let%test_unit "Test input sums to expected value" =
  [%test_eq: int] (read_calibration_corrected "inputs/day1_example.txt") 281