open Core

(** Map input lines from file_to_read to a list of values with a line_handler *) 
let map_file_input file_to_read line_handler =
    let lines = In_channel.read_lines file_to_read in
      List.map ~f: line_handler lines

(** Determine whether a character is a digit *)
let is_digit digit = match digit with
  '0' .. '9' -> true
  | _ -> false


(** Obtain the first and last items within a list *)
let rec first_last items = match items with
  | [] -> failwith "empty items"
  | [e] -> (e, e)
  | [e1;e2] -> (e1,e2) 
  | e1 :: _ :: r -> first_last (e1::r)

(** Get the first and last digits in a line *)
let first_last_digits line =
  String.to_list line |> List.filter ~f:is_digit |> first_last

(** Obtain the calibration value from a line *)
let get_calibration line =
  let (first, last) = first_last_digits line in
  String.of_char_list [first;last] |> int_of_string

(** Sum the calibration values for an input file *)
let read_calibration : string -> int = fun file_to_read ->
  let calibrations = map_file_input file_to_read get_calibration in
  List.fold_left calibrations ~init:0 ~f:(+)


(** Part two *)

(** Now multiple patterns can represent a digit. *)
let digit_patterns = [
  (("1", "one"), 1);
  (("2", "two"), 2);
  (("3", "three"), 3);
  (("4", "four"), 4);
  (("5", "five"), 5);
  (("6", "six"), 6);
  (("7", "seven"), 7);
  (("8", "eight"), 8);
  (("9", "nine"), 9)
] |>  List.concat_map ~f: (fun ((a, b), digit) ->
  let first = ((String.to_list a), digit) in
  let second = ((String.to_list b), digit) in
  [first;second])

(** Probably exists in a standard library somewhere, but who knows where? *)
let rec starts_with : char list -> char list -> bool = fun xs ys ->
  match ys with
  | [] -> true
  | y :: rys -> match xs with
    | x :: rxs -> Char.equal x y && (starts_with rxs rys)
    | _ -> false

(** Match digit patterns within a list of chars *)
let rec get_digits : char list -> int list = fun line_chars ->
  match line_chars with
  | [] -> []
  | _ :: rest ->
    let matched_pattern = List.find ~f: (fun (pattern, _) ->
      starts_with line_chars pattern) digit_patterns in
    match matched_pattern with
    | Some (_, digit) -> digit :: (get_digits rest)
    | _ -> get_digits rest


let first_last_digits_corrected line =
  String.to_list line |> get_digits |> first_last

let get_calibration_corrected line =
  let (first, last) = first_last_digits_corrected line in
  (first * 10) + last

let read_calibration_corrected = fun file_to_read ->
  let calibrations = map_file_input file_to_read get_calibration_corrected in
  List.fold_left calibrations ~init:0 ~f:(+)


let%test_unit "Test input sums to expected value" =
  [%test_eq: int] (read_calibration_corrected "../inputs/day1_example.txt") 281

let%test_unit "Examples" =
  [%test_eq: int] (get_calibration_corrected "onerubbish") 11;
  [%test_eq: int] (get_calibration_corrected "one") 11;
  [%test_eq: int] (get_calibration_corrected "rubbishone") 11;
  [%test_eq: int] (get_calibration_corrected "rubbishonerubbish") 11;
  [%test_eq: int] (get_calibration_corrected "9_one") 91;
  [%test_eq: int] (get_calibration_corrected "8_two") 82;
  [%test_eq: int] (get_calibration_corrected "7_three") 73;
  [%test_eq: int] (get_calibration_corrected "6_four") 64;
  [%test_eq: int] (get_calibration_corrected "5_five") 55;
  [%test_eq: int] (get_calibration_corrected "4_six") 46;
  [%test_eq: int] (get_calibration_corrected "3_seven") 37;
  [%test_eq: int] (get_calibration_corrected "2_eight") 28;
  [%test_eq: int] (get_calibration_corrected "1_nine") 19;
  [%test_eq: int] (get_calibration_corrected "twone") 21

let%test_unit "Selection from input" =
  [%test_eq: int] (get_calibration_corrected "29lzrxseven") 27;
  [%test_eq: int] (get_calibration_corrected "9nnqljsixkzphvtmtr") 96;
  [%test_eq: int] (get_calibration_corrected "five73kskpfgbkcltwoccvf") 52;
