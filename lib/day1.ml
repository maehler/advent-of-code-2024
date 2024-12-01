let parse filename = 
    let file = open_in filename in
    let rec parse_line acc1 acc2 =
        try
            let line = input_line file in
            match Str.split (Str.regexp " +") line with
            | [a; b] ->
                    parse_line ((int_of_string a) :: acc1) ((int_of_string b) :: acc2)
            | _ ->
                    parse_line acc1 acc2
        with
        | End_of_file ->
                close_in file;
                (List.rev acc1, List.rev acc2)
    in
    parse_line [] []

let diff list1 list2 =
    let list1_sorted = List.sort (fun x y -> if x < y then -1 else 1) list1 in
    let list2_sorted = List.sort (fun x y -> if x < y then -1 else 1) list2 in
    let rec calc_diff acc a b =
        match (a, b) with
        | (x :: rest1, y :: rest2) ->
                calc_diff (acc + abs (x - y)) rest1 rest2
        | ([], []) ->
                acc
        | _ -> assert false
    in
    let d = calc_diff 0 list1_sorted list2_sorted in
    print_endline (string_of_int d)

let part1 = fun input ->
    let (list1, list2) = parse input in
    diff list1 list2

let run  = fun input part ->
    match part with
    | 1 -> part1 input
    | 2 -> failwith "part 2 not yet implemented"
    | _ -> failwith "invalid part"
