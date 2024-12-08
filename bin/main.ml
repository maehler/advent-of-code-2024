let usage_msg = "usage: aoc <day> <file>"
let input_args = ref []
let day = ref 0
let part = ref 1
let input_file = ref ""
let speclist = [
    ("-p", Arg.Set_int part, "Which part to run (default 1)")
]
let anon_fun arg = input_args := arg :: !input_args

let () =
    try
        Arg.parse speclist anon_fun usage_msg;
        if !part != 1 && !part != 2 then failwith "part must be 1 or 2";
        match List.rev !input_args with
        | [] ->
                print_endline usage_msg;
                Printf.eprintf "no arguments given\n";
                exit 1
        | _ :: [] ->
                print_endline usage_msg;
                Printf.eprintf "missing input file\n";
                exit 1
        | sday :: sinput_file :: [] ->
                day := int_of_string sday;
                input_file := sinput_file;
                if !day < 1 || !day > 25 then failwith "day must be between 1 and 25";
        | _ :: _ :: rest ->
                print_endline usage_msg;
                Printf.eprintf "unknown arguments: %s\n" (String.concat ", " rest);
                exit 1
    with e ->
        Printf.eprintf "error: %s\n" (Printexc.to_string e);
        exit 1

let () =
    match !day with
    | 1 -> Aoc.Day1.run !input_file !part
    | 2 -> Aoc.Day2.run !input_file !part
    | 3 -> Aoc.Day3.run !input_file !part
    | 4 -> Aoc.Day4.run !input_file !part
    | d ->
            Printf.eprintf "day %d not yet implemented\n" d;
            exit 1
