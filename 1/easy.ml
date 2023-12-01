open Core

let rec solve sum =
  match In_channel.(input_line stdin) with
  | None -> sum
  | Some row ->
      let digits = String.filter row ~f:Char.is_digit in
      let first =
        Char.get_digit_exn @@ Option.value_exn @@ List.hd
        @@ String.to_list digits
      in
      let last =
        Char.get_digit_exn @@ Option.value_exn @@ List.hd
        @@ String.to_list_rev digits
      in
      solve (sum + ((first * 10) + last))

let () = print_endline @@ Int.to_string @@ solve 0
