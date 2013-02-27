(*********************************************************************************)
(*                Genet                                                          *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License version 3             *)
(*    or later as published by the Free Software Foundation.                     *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software Foundation,    *)
(*    Inc., 59 Temple Place, Suite 330, Boston, MA                               *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(** *)

type completion_choices =
  { compl_words : string list ;
    compl_files : bool ;
    compl_xfiles : string option ;
  }

let compl_choices ?(words=[]) ?(files=false) ?xfiles () =
  { compl_words = words ;
    compl_files = files ;
    compl_xfiles = xfiles ;
  }
;;

type completion_fun = (unit -> completion_choices)
type completion_opt = completion_fun option

type spec =
  | Unit of (unit -> unit)
  | Bool of (bool -> unit)
  | Set of bool Pervasives.ref
  | Clear of bool Pervasives.ref
  | String of completion_opt * (string -> unit)
  | Set_string of completion_opt * string Pervasives.ref
  | Int of completion_opt * (int -> unit)
  | Set_int of completion_opt * int Pervasives.ref
  | Float of completion_opt * (float -> unit)
  | Set_float of completion_opt * float Pervasives.ref
  | Tuple of Arg.spec list
  | Symbol of string list * (string -> unit)
  | Rest of (string -> unit)

let spec_to_arg_spec = function
| Unit f -> Arg.Unit f
| Bool f -> Arg.Bool f
| Set r -> Arg.Set r
| Clear r -> Arg.Clear r
| String (_, f) -> Arg.String f
| Set_string (_, r) -> Arg.Set_string r
| Int (_, f) -> Arg.Int f
| Set_int (_, r) -> Arg.Set_int r
| Float (_, f) -> Arg.Float f
| Set_float (_, r) -> Arg.Set_float r
| Tuple l -> Arg.Tuple l
| Symbol (l, f) -> Arg.Symbol (l, f)
| Rest f -> Arg.Rest f
;;

let specs_to_arg_specs =
  let f (s1, spec, s2) = (s1, spec_to_arg_spec spec, s2) in
  List.map f
;;

type option_spec = string * spec * string;;

type completion_spec =
  | Complist of completion_fun
  | Compfun of completion_fun
;;

type command_kind =
| Final of (unit -> unit)
| Commands of (string * command * string) list

and command = {
  com_options : option_spec list ;
  com_usage : string ;
  com_compl : completion_spec list ;
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
    let coms = List.sort
      (fun (s1, _, _) (s2, _ ,_) -> Pervasives.compare s1 s2)
      coms
    in
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

let parse ?(args=Sys.argv) com =
  let remaining = ref [] in
  let rec iter ?(path=[]) ?(options=[]) ?(argv=args) com =
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
    let arg_options = specs_to_arg_specs options in
    try
      match com.com_kind with
        Final f ->
          let anon_fun s = remaining := s :: !remaining in
          Arg.parse_argv ~current argv arg_options anon_fun com.com_usage;
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
            Arg.parse_argv ~current argv arg_options anon_fun com.com_usage;
            raise (Missing_command path)
          with
            Stop_parse f ->
              f ()
    with
      Arg.Help _ ->
        let msg = make_help_msg path arg_options com.com_usage com.com_kind in
        raise (My_help msg)
  in
  try iter ~argv: args com; !remaining
  with
    Arg.Bad s -> failwith s
  | My_help s -> prerr_endline s; exit 0
  | No_such_command path ->
      failwith (Printf.sprintf "No such command: %s" (String.concat " " path))
  | Missing_command path ->
      failwith (Printf.sprintf "%s: please give a subcommand" (String.concat " " path))
;;

let completion stop args com =
  let len = Array.length args in
  let mk_choices options com =
    let f (s,_,_) = s in
    let choices =
      (List.map f options) @
        (match com.com_kind with
           Final _ -> []
         | Commands l -> List.map f l)
    in
    match com.com_compl with
      [] -> compl_choices ~words: choices ()
    | h :: q ->
        let spec =
          match h with
            Compfun f
          | Complist f -> f ()
        in
        { spec with compl_words = choices @ spec.compl_words }
  in
  let rec iter_option_param pos options com op_spec =
    if pos = stop then
        match op_spec with
        | String (Some f, _)
        | Set_string (Some f, _)
        | Int (Some f, _)
        | Set_int (Some f, _)
        | Float (Some f, _)
        | Set_float (Some f, _) -> f ()
        | Bool _ -> compl_choices ~words: ["true" ; "false"] ()
        | Symbol (words, _) -> compl_choices ~words ()
        | Tuple _ -> compl_choices () (* FIXME: Arg.Tuple not handled *)
        | _ -> iter pos options com
    else
      if pos >= len then
        iter pos options com
      else
        match op_spec with
        | String _ | Set_string _
        | Int _ | Set_int _
        | Float _ | Set_float _
        | Bool _ | Tuple _ | Symbol _ ->
            iter (pos+1) options com
        | _ ->
            iter pos options com

  and iter pos options com =
    match com.com_kind with
      Final _ ->
        if pos >= len || pos = stop then
          mk_choices options com
        else
          begin
            let arg = args.(pos) in
            try
              let (_,k,_) = List.find (fun (s, _, _) -> s=arg) options in
              iter_option_param (pos+1) options com k
            with
              Not_found ->
                  let com =
                  match com.com_compl with
                    [] -> com
                  | (Compfun _) :: q ->
                      { com with com_compl = q }
                  | (Complist _) :: _ -> com
                in
                iter (pos+1) options com
          end
    | Commands coms ->
        if pos >= len || pos = stop then
             mk_choices options com
        else
          begin
            let arg = args.(pos) in
            try
              let (_,k,_) = List.find (fun (s, _, _) -> s=arg) options in
              iter_option_param (pos+1) options com k
            with
              Not_found ->
                try
                  let (_,com,_) = List.find (fun (s, _, _) -> s=arg) coms in
                  let options = merge_options ~options ~more: com.com_options in
                  iter (pos+1) options com
                with
                  Not_found ->
                    compl_choices ()
          end
  in
  iter 1 com.com_options com
;;
