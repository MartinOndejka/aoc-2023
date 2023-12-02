open Core
module M = String.Map

let parse_row row =
  let _, sets = String.lsplit2_exn row ~on:':' in
  String.split sets ~on:';'
  |> List.map ~f:String.strip
  |> List.map ~f:(fun set ->
         String.strip set
         |> String.split ~on:','
         |> List.map ~f:String.strip
         |> List.map ~f:(fun color ->
                let count, color = String.lsplit2_exn color ~on:' ' in
                let count = Int.of_string count in
                (count, color)))

let rec solve sum =
  match In_channel.(input_line stdin) with
  | None -> sum
  | Some row ->
      let sets = parse_row row in
      let map =
        List.join sets
        |> List.fold ~init:M.empty ~f:(fun map (count, color) ->
               match M.find map color with
               | None -> M.set map ~key:color ~data:count
               | Some old_count ->
                   M.set map ~key:color ~data:(max old_count count))
      in
      let red = M.find_exn map "red" in
      let green = M.find_exn map "green" in
      let blue = M.find_exn map "blue" in
      solve (sum + (red * green * blue))

let () = print_endline @@ Int.to_string @@ solve 0
