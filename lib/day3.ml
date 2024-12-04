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

let calc_mul mul =
    match mul with
    | a :: b :: [] -> (int_of_string a) * (int_of_string b)
    | _ -> assert false

let part1 input =
    let muls = read input |> List.map parse |> List.concat in
    List.map calc_mul muls |> List.fold_left (+) 0 |> Printf.printf "%d\n"

let skip_donts line =
    let split_regex = Str.regexp {|mul([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?)\|do()\|don't()|} in
    let rec filter enabled acc npos =
        try
            let pos = Str.search_forward split_regex line npos in
            let m = Str.matched_string line in
            match enabled, m with
            | _, "don't()" -> filter false acc (pos + 1)
            | _, "do()" -> filter true acc (pos + 1)
            | false, _ -> filter enabled acc (pos + 1)
            | true, s -> filter enabled (s :: acc) (pos + 1)
        with Not_found -> List.rev acc
    in
    let muls = filter true [] 0 in
    muls

let part2 input =
    let muls = read input |> String.concat "" |> skip_donts |> List.map parse |> List.concat in
    let e = List.map calc_mul muls in
    List.fold_left (+) 0 e |> Printf.printf "%d\n"

let run input part =
    match part with
    | 1 -> part1 input
    | 2 -> part2 input
    | _ -> failwith "invalid part"
