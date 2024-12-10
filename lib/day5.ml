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

let fix_update rules unordered_update =
    let rec aux acc update =
        if (Set.length update) = 0 then List.rev acc
        else
            let next = Set.find_exn update ~f:(fun n1 ->
                not(Set.exists update ~f:(fun n2 ->
                    List.mem rules (n2, n1) ~equal:(fun (a, b) (a2, b2) -> a = a2 && b = b2)
                ))) in
            aux (next :: acc) (Set.remove update next)
    in
    let ordered_update = aux [] (Set.of_list (module Int) unordered_update) in
    assert (is_valid_update rules ordered_update);
    ordered_update

let part1 filename =
    let (rules, updates) = read filename |> parse in
    let valid_updates = List.map updates ~f:(fun u -> is_valid_update rules u) in
    List.filteri updates ~f:(fun i _ -> List.nth_exn valid_updates i) |>
    List.map ~f:(fun update ->
        List.nth_exn update ((List.length update) / 2)
    ) |>
    List.fold_left ~f:(+) ~init:0 |>
    Printf.printf "%d\n"

let part2 filename = 
    let (rules, updates) = read filename |> parse in
    let invalid_updates = List.map updates ~f:(fun u -> not (is_valid_update rules u)) in
    List.filteri updates ~f:(fun i _ -> List.nth_exn invalid_updates i) |>
    List.map ~f:(fun u -> fix_update rules u) |>
    List.map ~f:(fun update ->
        List.nth_exn update ((List.length update) / 2)
    ) |>
    List.fold_left ~f:(+) ~init:0 |>
    Printf.printf "%d\n"

let run filename part =
    match part with
    | 1 -> part1 filename
    | 2 -> part2 filename
    | _ -> failwith "invalid part"
