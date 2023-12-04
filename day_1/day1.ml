open Batteries

let fix_line line =
  let clean_line = Str.global_replace (Str.regexp "[a-zA-Z]") "" line in
  let first_char = String.get clean_line 0 in
  let last_char = String.get clean_line (String.length clean_line - 1) in
  int_of_string (String.of_char first_char ^ String.of_char last_char)

let () =
  let filelines = File.lines_of "puzzle.txt" in
  let lines_list = Enum.fold (fun acc line -> (fix_line line) :: acc) [] filelines in
  let total_sum = List.fold_left (fun acc x -> acc + x) 0 lines_list in
  Printf.printf "%d\n" total_sum