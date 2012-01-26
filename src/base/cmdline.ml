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
    Arg.usage_string options usage
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
    Arg.usage_string options usage
;;

exception My_help of string;;

let parse com =
  let remaining = ref [] in
  let current = ref 0 in
  let rec iter ?(path=[]) ?(options=[]) ?(argv=Sys.argv) com =
    let path = path @ [argv.(0)] in
(*    prerr_endline
    (Printf.sprintf "iter argv=%s"
      (String.concat " " (Array.to_list argv)));
    prerr_endline (Printf.sprintf "path=%s" (String.concat "/" path));
*)
    let options = merge_options ~options ~more: com.com_options in
    let anon_fun =
      match com.com_kind with
        Final _ -> (fun s -> remaining := s :: !remaining)
      | Commands subs ->
          let f s =
            try
              let (_,com,_) = List.find (fun (name,_,_) -> s = name) subs in
              iter ~path ~options
              ~argv: (Array.sub argv !current (Array.length argv - !current))
              com
            with Not_found ->
              raise (No_such_command (path @ [s]))
          in
          f
    in
    current := 0;
    begin
      try Arg.parse_argv ~current argv options anon_fun com.com_usage;
      with Arg.Help _ ->
          let msg = make_help_msg path options com.com_usage com.com_kind in
          raise (My_help msg)
    end;
    match com.com_kind with
      Final f -> f ()
    | _ -> ()
  in
  try iter com
  with
    Arg.Bad s -> failwith s
  | My_help s -> prerr_endline s; exit 0
  | No_such_command path ->
      failwith (Printf.sprintf "No such command: %s" (String.concat " " path))
;;

