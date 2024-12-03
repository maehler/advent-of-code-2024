let read filename =
    let file = open_in filename in
    let rec read_line acc =
        try
            let line = input_line file in
            read_line (line :: acc)
        with
        | End_of_file ->
                close_in_noerr file;
                List.rev acc
    in
    read_line []

let parse line =
    let mul_regex = Str.regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|} in
    let rec find_all acc pos =
        match Str.search_forward mul_regex line pos with
        | p -> find_all ([(Str.matched_group 1 line); (Str.matched_group 2 line)] :: acc) (p + 1)
        | exception Not_found -> List.rev acc
    in
    find_all [] 0

let part1 input =
    let muls = read input |> List.map parse |> List.concat in
    List.map (fun x ->
        match x with
        | a :: b :: [] -> (int_of_string a) * (int_of_string b)
        | _ -> assert false
    ) muls |> List.fold_left (+) 0 |> Printf.printf "%d\n"

let run input part =
    match part with
    | 1 -> part1 input
    | _ -> failwith "invalid part"
