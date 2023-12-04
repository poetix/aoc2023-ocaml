open Aoc2023.Day1
open Aoc2023.Day2

let () =
  let day1_part1 = read_calibration "inputs/day1.txt" in
  let day1_part2 = read_calibration_corrected "inputs/day1.txt" in
  Printf.printf "Day 1, part 1: %d, Part 2: %d\n" day1_part1 day1_part2;

  let day2_part1 = sum_possible_game_ids "inputs/day2.txt" in
  let day2_part2 = sum_powers "inputs/day2.txt" in
  Printf.printf "Day 2, part 1: %d, part 2: %d" day2_part1 day2_part2;
