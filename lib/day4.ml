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

let part1 filename =
    read filename |> parse_board |> count_xmas |> Printf.printf "%d\n"

let run filename part =
    match part with
    | 1 -> part1 filename
    | _ -> failwith "invalid part"
