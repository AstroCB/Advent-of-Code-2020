(* --- Day 5: Binary Boarding ---

   You board your plane only to discover a new problem: you dropped your boarding pass! You aren't sure which seat is yours, and all of the flight attendants are busy with the flood of people that suddenly made it through passport control.

   You write a quick program to use your phone's camera to scan all of the nearby boarding passes (your puzzle input); perhaps you can find your seat through process of elimination.

   Instead of zones or groups, this airline uses binary space partitioning to seat people. A seat might be specified like FBFBBFFRLR, where F means "front", B means "back", L means "left", and R means "right".

   The first 7 characters will either be F or B; these specify exactly one of the 128 rows on the plane (numbered 0 through 127). Each letter tells you which half of a region the given seat is in. Start with the whole list of rows; the first letter indicates whether the seat is in the front (0 through 63) or the back (64 through 127). The next letter indicates which half of that region the seat is in, and so on until you're left with exactly one row.

   For example, consider just the first seven characters of FBFBBFFRLR:

   Start by considering the whole range, rows 0 through 127.
   F means to take the lower half, keeping rows 0 through 63.
   B means to take the upper half, keeping rows 32 through 63.
   F means to take the lower half, keeping rows 32 through 47.
   B means to take the upper half, keeping rows 40 through 47.
   B keeps rows 44 through 47.
   F keeps rows 44 through 45.
   The final F keeps the lower of the two, row 44.
   The last three characters will be either L or R; these specify exactly one of the 8 columns of seats on the plane (numbered 0 through 7). The same process as above proceeds again, this time with only three steps. L means to keep the lower half, while R means to keep the upper half.

   For example, consider just the last 3 characters of FBFBBFFRLR:

   Start by considering the whole range, columns 0 through 7.
   R means to take the upper half, keeping columns 4 through 7.
   L means to take the lower half, keeping columns 4 through 5.
   The final R keeps the upper of the two, column 5.
   So, decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.

   Every seat also has a unique seat ID: multiply the row by 8, then add the column. In this example, the seat has ID 44 * 8 + 5 = 357.

   Here are some other boarding passes:

   BFFFBBFRRR: row 70, column 7, seat ID 567.
   FFFBBBFRRR: row 14, column 7, seat ID 119.
   BBFFBBFRLL: row 102, column 4, seat ID 820.
   As a sanity check, look through your list of boarding passes. What is the highest seat ID on a boarding pass? *)

let rec guided_binary_search lower_letter upper_letter lower_range upper_range instrs =
  let midpoint = lower_range + ((upper_range - lower_range) / 2) in
  match instrs with
  | [instr] when instr = lower_letter -> lower_range
  | [instr] when instr = upper_letter -> upper_range
  | instr::t when instr = lower_letter -> guided_binary_search lower_letter upper_letter lower_range midpoint t
  | instr::t when instr = upper_letter -> guided_binary_search lower_letter upper_letter (midpoint+1) upper_range t
  | _ -> raise (Invalid_argument "bad instr string")
;;

let n_rows = 128;;
let n_cols = 8;;

(* Searching for a row uses F to indicate choosing the lower range and B to indicate the upper range with bounds 0 - 127 *)
let locate_row = guided_binary_search "F" "B" 0 (n_rows-1);;
(* Searching for a col uses L to indicate choosing the lower range and R to indicate the upper range with bounds 0 - 7 *)
let locate_col = guided_binary_search "L" "R" 0 (n_cols-1);;

let get_id row col = row * n_cols + col;;

let seat_id instrs =
  let instr_list = (Str.split (Str.regexp "") instrs) in
  let rows = List.filter (fun i -> i = "F" || i = "B") instr_list in
  let cols = List.filter (fun i -> i = "L" || i = "R") instr_list in
  let row = locate_row rows in
  let col = locate_col cols in
  get_id row col
;;

let highest_id passes = 
  List.fold_left (fun a pass -> let id = seat_id pass in
                   if id > a then id else a
                 ) 0 passes
;;

(* --- Part Two ---

   Ding! The "fasten seat belt" signs have turned on. Time to find your seat.

   It's a completely full flight, so your seat should be the only missing boarding pass in your list. However, there's a catch: some of the seats at the very front and back of the plane don't exist on this aircraft, so they'll be missing from your list as well.

   Your seat wasn't at the very front or back, though; the seats with IDs +1 and -1 from yours will be in your list.

   What is the ID of your seat? *)

let my_id passes = 
  let ids = List.sort Pervasives.compare (List.map seat_id passes) in
  let (_, missing) = List.fold_left (fun (last, stored) id -> if id != (last+1) then (id, id-1) else (id, stored)) (-1, -1) ids in
  missing
;;

(* https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/5775024#5775024 *)
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines
;;

let lines = read_file "input.txt" in
print_string ("Part 1: " ^ (string_of_int (highest_id lines)));
print_newline ();
print_string ("Part 2: " ^ (string_of_int (my_id lines)))