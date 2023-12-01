open Core

let digits =
  [
    "zero";
    "one";
    "two";
    "three";
    "four";
    "five";
    "six";
    "seven";
    "eight";
    "nine";
  ]

let reversed_digits = List.map digits ~f:String.rev

let rec get_first_digit digits str =
  match
    List.findi digits ~f:(fun _ digit -> String.is_prefix str ~prefix:digit)
  with
  | Some (digit, _) -> digit
  | None -> (
      match String.to_list str with
      | [] -> failwith "String without any digit"
      | digit :: _ when Char.is_digit digit -> Char.get_digit_exn digit
      | _ :: rest -> get_first_digit digits (String.of_char_list rest))

let rec solve sum =
  match In_channel.(input_line stdin) with
  | None -> sum
  | Some row ->
      let first = get_first_digit digits row in
      let last = get_first_digit reversed_digits (String.rev row) in
      let value = (first * 10) + last in
      solve (sum + value)

let () = print_endline @@ Int.to_string @@ solve 0
