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
    | [] -> assert false

let rec is_safe_incr report =
    match report with
    | a :: b :: rest ->
            if b - a >= 1 && b - a <= 3 then is_safe_incr (b :: rest) else false
    | _ :: [] -> true
    | [] -> assert false

let is_safe report =
    match report with
    | _ :: _ :: _ ->
            if is_safe_incr report then true
            else if is_safe_decr report then true
            else false
    | _ :: [] -> true
    | [] -> assert false

let part1 input =
    let reports = parse input in
    let valid = List.map is_safe reports in
    let count = List.fold_left (fun acc x -> if x then acc + 1 else acc) 0 valid in
    Printf.printf "%d\n" count

let dampened_combos report =
    let rec combos prev next acc =
        match next with
        | [] -> acc
        | hd :: rest -> combos (hd :: prev) rest (List.append (List.rev prev) rest :: acc)
    in
    report :: combos [] report []

let is_safe_dampened report =
    let combos = dampened_combos report in
    let safe_combos = List.map is_safe combos in
    List.fold_left (||) false safe_combos

let part2 input = 
    let reports = parse input in
    let filtered_reports = List.filter is_safe_dampened reports in
    List.length filtered_reports |> Printf.printf "%d\n"

let run input part =
    match part with
    | 1 -> part1 input
    | 2 -> part2 input
    | _ -> failwith "invalid part"
