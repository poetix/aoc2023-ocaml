open Aoc2023.Day1

let () =
  let day1_part1 = read_calibration "inputs/day1.txt" in
  let day1_part2 = read_calibration_corrected "inputs/day1.txt" in
  Printf.printf "Part 1: %d, Part 2: %d\n" day1_part1 day1_part2
