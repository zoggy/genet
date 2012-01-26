(** *)

type option_spec = string * Arg.spec * string;;

type command_kind =
| Final of (unit -> unit)
| Commands of (string * command * string) list

and command = {
  com_options : option_spec list ;
  com_usage : string ;
  com_kind : command_kind ;
  }

let merge_options ~options ~more =
  let pred (option, _, _) = not (List.exists (fun (o,_,_) -> o = option) more) in
  (List.filter pred options) @ more
;;

exception No_such_command of string list

let align_commands coms =
  let max_width = List.fold_left
    (fun acc (s,_,_) -> max acc (String.length s))
    min_int coms
  in
  let f (s, com, desc) =
    let s = Printf.sprintf "%s%s"
      s (String.make (max_width - String.length s) ' ')
    in
    (s, com, desc)
  in
  List.map f coms
;;

let make_help_msg path options com_usage = function
  Final _ ->
    let usage = Printf.sprintf "Usage: %s [options] %s\nwhere options are:"
      (String.concat " " path) com_usage
    in
    Arg.usage_string (Arg.align options) usage
| Commands coms ->
    let usage = Printf.sprintf
      "Usage: %s [options] <command>\nwhere command can be:\n%s\n\nand options are:"
      (String.concat " " path)
      (String.concat "\n"
        (List.map
        (fun (name, _, desc) -> Printf.sprintf "  %s  %s" name desc)
        (align_commands coms)
       )
      )
    in
    Arg.usage_string (Arg.align options) usage
;;

exception My_help of string;;
exception Missing_command of string list;;
exception Stop_parse of (unit -> unit);;

let parse com =
  let remaining = ref [] in
  let rec iter ?(path=[]) ?(options=[]) ?(argv=Sys.argv) com =
    let current = ref 0 in
    let path = path @ [argv.(0)] in
(*
    prerr_endline
    (Printf.sprintf "iter argv=%s"
      (String.concat " " (Array.to_list argv)));
    prerr_endline (Printf.sprintf "path=%s" (String.concat "/" path));
    prerr_endline (Printf.sprintf "current=%d" !current);
*)
    let options = merge_options ~options ~more: com.com_options in
    try
      match com.com_kind with
        Final f ->
          let anon_fun s = remaining := s :: !remaining in
          Arg.parse_argv ~current argv options anon_fun com.com_usage;
          f ()
        | Commands subs ->
          let anon_fun s =
            try
              let (_,com,_) = List.find (fun (name,_,_) -> s = name) subs in
              let argv =
                Array.sub argv !current (Array.length argv - !current)
              in
              let f () = iter ~path ~options ~argv com in
              raise (Stop_parse f)
            with Not_found ->
                raise (No_such_command (path @ [s]))
          in
          try
            Arg.parse_argv ~current argv options anon_fun com.com_usage;
            raise (Missing_command path)
          with
            Stop_parse f -> f ()
    with
      Arg.Help _ ->
        let msg = make_help_msg path options com.com_usage com.com_kind in
        raise (My_help msg)
  in
  try iter com; !remaining
  with
    Arg.Bad s -> failwith s
  | My_help s -> prerr_endline s; exit 0
  | No_such_command path ->
      failwith (Printf.sprintf "No such command: %s" (String.concat " " path))
  | Missing_command path ->
      failwith (Printf.sprintf "%s: please give a subcommand" (String.concat " " path))
;;

