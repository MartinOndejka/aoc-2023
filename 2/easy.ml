open Core

let max_per_color = function
  | "red" -> 12
  | "green" -> 13
  | "blue" -> 14
  | _ -> failwith "Invalid color"

let parse_row row =
  let game, sets = String.lsplit2_exn row ~on:':' in
  let game_id = Int.of_string @@ String.chop_prefix_exn game ~prefix:"Game " in
  match
    String.split sets ~on:';' |> List.map ~f:String.strip
    |> List.map ~f:(fun set ->
           let set =
             List.map ~f:String.strip @@ String.split ~on:','
             @@ String.strip set
           in
           List.exists set ~f:(fun color ->
               let count, color = String.lsplit2_exn color ~on:' ' in
               let count = Int.of_string count in
               count > max_per_color color))
    |> List.exists ~f:ident
  with
  | true -> 0
  | false -> game_id

let rec solve sum =
  match In_channel.(input_line stdin) with
  | None -> sum
  | Some row -> solve (sum + parse_row row)

let () = print_endline @@ Int.to_string @@ solve 0
