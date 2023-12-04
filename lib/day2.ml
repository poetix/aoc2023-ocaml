open Core
open Angstrom

let integer = take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
let game_label = Angstrom.string "Game " *> integer <* char ':'

type bead_colour_literal = Red | Green | Blue
[@@deriving show, eq]


let red = Angstrom.string "red" *> return Red
let green = Angstrom.string "green" *> return Green
let blue = Angstrom.string "blue" *> return Blue

let bead_colour = red <|> green <|> blue

let whitespace = many (char ' ')
let pair a b = (a, b)
let reverse_pair a b = (b, a)

let bead_count = lift2 reverse_pair (whitespace *> integer <* whitespace) bead_colour

let comma = char ','
let turn = sep_by comma bead_count

let semicolon = char ';'
let turns = sep_by semicolon turn

let game = lift2 pair game_label turns

let count_or_0 : (bead_colour_literal * int) list -> bead_colour_literal -> int = fun draws colour ->
  List.Assoc.find ~equal:equal_bead_colour_literal draws colour |> (function Some n -> n | _ -> 0)

let max_beads bead_type turns = 
  let max_int ints = List.max_elt ~compare:Int.compare ints |> (function Some n -> n | _ -> 0) in
  List.map ~f: (fun turn -> count_or_0 turn bead_type) turns |> max_int

let is_possible : (bead_colour_literal * int) list list -> bool = fun turns ->
  let max_red = max_beads Red turns in
  let max_green = max_beads Green turns in
  let max_blue = max_beads Blue turns in
  max_red <= 12 && max_green <= 13 && max_blue <= 14

let line_reader line = 
  match parse_string ~consume:All game line with
  | Ok v -> v
  | Error msg -> failwith msg

let sum_possible_game_ids file =
  let games = Day1.map_file_input file line_reader in
  let possibles = List.filter ~f:(fun (_, turns) -> is_possible turns) games in
  let ids = List.map ~f:(fun (id, _) -> id) possibles in
  List.fold_left ~f:(+) ~init:0 ids

(** Day 2*)
let power : (bead_colour_literal * int) list list -> int = fun turns ->
  let max_red = max_beads Red turns in
  let max_green = max_beads Green turns in
  let max_blue = max_beads Blue turns in
  max_red * max_green * max_blue

let sum_powers file = 
  let games = Day1.map_file_input file line_reader in
  let powers = List.map ~f:(fun (_, turns) -> power turns) games in
  List.fold_left ~f:(+) ~init:0 powers