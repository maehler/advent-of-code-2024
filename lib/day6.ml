open Core
open Utils

module Coordmap = Map.Make(Coordinate)
module Coordset = Set.Make(Coordinate)

module Direction = struct
    type dir =
        | Up
        | Right
        | Down
        | Left

    let of_char d =
        match d with
        | '^' -> Up
        | '>' -> Right
        | 'v' -> Down
        | '<' -> Left
        | _ -> assert false

    let to_coord d =
        match d with
        | Up -> (0, -1)
        | Right -> (1, 0)
        | Down -> (0, 1)
        | Left -> (-1, 0)

    let to_string d =
        match d with
        | Up -> "up"
        | Right -> "right"
        | Down -> "down"
        | Left -> "left"

    let turn_right d =
        match d with
        | (0, -1) -> (1, 0)
        | (1, 0) -> (0, 1)
        | (0, 1) -> (-1, 0)
        | (-1, 0) -> (0, -1)
        | _ -> assert false
end 

module Board = struct
    type map_object =
        | Guard of Direction.dir
        | Obstacle
        | Empty

    let to_string o =
        match o with
        | Guard _ -> "guard"
        | Obstacle -> "#"
        | Empty -> "."

    let guard_direction g =
        match g with
        | Guard x -> x
        | _ -> assert false
end

let parse filename =
    In_channel.read_all filename |>
    String.split_lines |>
    List.foldi ~init:(Coordmap.empty) ~f:(fun y acc line ->
        String.foldi line ~init:acc ~f:(fun x acc c ->
            let obj = match c with
            | '^' | '>' | 'v' | '<' -> Board.Guard (Direction.(of_char) c)
            | '#' -> Board.Obstacle
            | '.' -> Board.Empty
            | _ -> assert false
            in
            Map.add_exn ~key:(x, y) ~data:obj acc
        ) 
    )

let part1 filename =
    let board = parse filename in
    let guard = Map.filter board ~f:(fun v ->
        match v with
        | Board.Guard _ -> true
        | _ -> false
    ) in
    let visited = Set.of_map_keys guard in
    let guard_pos = Set.to_list visited |> List.hd_exn in
    let guard_dir = Map.find_exn board guard_pos |> Board.guard_direction |> Direction.to_coord in
    let rec step acc (x, y) (dx, dy) =
        let next_pos = (x + dx, y + dy) in
        match Map.find board next_pos with
        | Some Board.Obstacle ->
                step (Set.add acc (x, y)) (x, y) (Direction.turn_right (dx, dy))
        | Some (Board.Guard _) | Some Board.Empty ->
                step (Set.add acc (x, y)) next_pos (dx, dy)
        | None ->
                Set.add acc (x, y)

    in
    step visited guard_pos guard_dir |>
    Set.length |>
    Printf.printf "%d\n"


let run filename part =
    match part with
    | 1 -> part1 filename
    | _ -> failwith "invalid part"
