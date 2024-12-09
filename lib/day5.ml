open Core

let read filename = 
    In_channel.read_all filename

let parse s =
    let lines = String.split_lines s in
    let rec get_order_rules rules lines =
        match lines with
        | "" :: tl -> rules, tl
        | line :: tl ->
                let nums = String.split_on_chars line ~on:['|'] |> List.map ~f:int_of_string in
                let rule = match nums with
                | [a; b] -> (a, b)
                | _ -> assert false
                in
                get_order_rules (rule :: rules) tl
        | _ -> assert false
    in
    let rules, lines = get_order_rules [] lines in
    let updates = List.map ~f:(fun lst -> String.split_on_chars ~on:[','] lst |> List.map ~f:int_of_string) lines in
    (rules, updates)

let is_valid_update rules update =
    let rec pairs acc lst =
        match lst with
        | _ :: [] -> acc
        | [] -> acc
        | hd :: tl -> pairs (acc @ (List.map tl ~f:(fun x -> (hd, x)))) tl
    in
    List.for_all (pairs [] update) ~f:(fun (first, second) ->
        not (List.mem rules (second, first) ~equal:(fun (a, b) (a2, b2) -> a = a2 && b = b2)))

let part1 filename =
    let (rules, updates) = read filename |> parse in
    let valid_updates = List.map updates ~f:(fun u -> is_valid_update rules u) in
    List.filteri updates ~f:(fun i _ -> List.nth_exn valid_updates i) |>
    List.map ~f:(fun update ->
        let len = List.length update in
        let middle = List.nth_exn update (len / 2) in
        middle
    ) |>
    List.fold_left ~f:(+) ~init:0 |>
    Printf.printf "%d\n"

let run filename part =
    match part with
    | 1 -> part1 filename
    | _ -> failwith "invalid part"
