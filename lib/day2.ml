let parse filename =
    let file = open_in filename in
    let rec parse_line acc =
        try
            let line = input_line file in
            match line with
            | line ->
                    let cols = String.split_on_char ' ' line in
                    let intcols = List.map int_of_string cols in
                    parse_line (intcols :: acc)
        with
        | End_of_file ->
                close_in file;
                List.rev acc
    in
    parse_line []

let rec is_safe_decr report =
    match report with
    | a :: b :: rest ->
            if a - b >= 1 && a - b <= 3 then is_safe_decr (b :: rest) else false
    | _ :: [] -> true
    | [] -> true

let rec is_safe_incr report =
    match report with
    | a :: b :: rest ->
            if b - a >= 1 && b - a <= 3 then is_safe_incr (b :: rest) else false
    | _ :: [] -> true
    | [] -> true

let is_safe report =
    match report with
    | a :: b :: _ ->
            if b > a then is_safe_incr report
            else if b < a then is_safe_decr report
            else false
    | _ :: [] -> true
    | [] -> true

let part1 input =
    let reports = parse input in
    let valid = List.map is_safe reports in
    let count = List.fold_left (fun acc x -> if x then acc + 1 else acc) 0 valid in
    Printf.printf "%d\n" count

let run input part =
    match part with
    | 1 -> part1 input
    | 2 -> failwith "not implemented yet"
    | _ -> failwith "invalid part"
