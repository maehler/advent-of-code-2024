open! Core
open! Utils

module Coordmap = Map.Make(Coordinate)

let offset8 = [
    (-1, -1);
    (0, -1);
    (1, -1);
    (1, 0);
    (1, 1);
    (0, 1);
    (-1, 1);
    (-1, 0);
]

let read filename =
    In_channel.read_all filename

let parse_board s =
    String.split_lines s |> List.foldi ~init:Coordmap.empty ~f:(fun y acc line ->
        String.foldi line ~init:acc ~f:(fun x acc c ->
            Map.add_exn ~key:(x, y) ~data:c acc
        )
    )

let count_xmas board =
    let get_word length start direction =
        let (sx, sy) = start in
        let (dx, dy) = direction in
        List.map (List.range 0 length) ~f:(fun i ->
            let pos = (sx + dx * i, sy + dy * i) in
            match Map.find board pos with
            | Some c -> c
            | None -> '.'
        ) |> String.of_list
    in
    let words = Map.fold board ~init:[] ~f:(fun ~key:(x, y) ~data:c acc ->
        match c with
        | 'X' -> (List.map offset8 ~f:(get_word 4 (x, y))) @ acc
        | _ -> acc
    ) in
    List.filter ~f:(fun w -> (String.compare w "XMAS") = 0) words |>
    List.length

let count_x_mas board = 
    let get_cross start = 
        let (x, y) = start in
        let offsets1 = [(-1, -1); (0, 0); (1, 1)] in
        let offsets2 = [(1, -1); (0, 0); (-1, 1)] in
        let first = List.map offsets1 ~f:(fun (dx, dy) ->
            let pos = (x + dx, y + dy) in
            match Map.find board pos with
            | Some c -> c
            | None -> '.'
        ) |> String.of_list in
        let second = List.map offsets2 ~f:(fun (dx, dy) ->
            let pos = (x + dx, y + dy) in
            match Map.find board pos with
            | Some c -> c
            | None -> '.'
        ) |> String.of_list in
        (first, second)
    in
    let crosses = Map.fold board ~init:[] ~f:(fun ~key:(x, y) ~data:c acc ->
        match c with
        | 'A' -> (get_cross (x, y)) :: acc
        | _ -> acc
    ) in
    List.filter crosses ~f:(fun (w1, w2) ->
        match w1, w2 with
        | "SAM", "SAM" -> true
        | "SAM", "MAS" -> true
        | "MAS", "SAM" -> true
        | "MAS", "MAS" -> true
        | _ -> false
    ) |> List.length

let part1 filename =
    read filename |> parse_board |> count_xmas |> Printf.printf "%d\n"

let part2 filename =
    read filename |> parse_board |> count_x_mas |> Printf.printf "%d\n"

let run filename part =
    match part with
    | 1 -> part1 filename
    | 2 -> part2 filename
    | _ -> failwith "invalid part"
