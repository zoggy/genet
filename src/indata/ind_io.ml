(** *)

module CF = Config_file;;
open Ind_types;;

let input_basename = "spec.in";;

type error = (string * string) (** file * msg *)
exception Error of error

let error ~file msg = raise (Error (file, msg));;

let string_of_error (file, msg) =
  Printf.sprintf "File %s: %s" file msg
;;

type input =
{ group : CF.group ;
  in_cp : string list CF.cp ;
  chains_cp : string list CF.cp ;
}

let mk_spec_group () =
  let group = new CF.group in
  let in_cp = new CF.list_cp CF.string_wrappers ~group ["in"] []
    "Input files and directories; order matters"
  in
  let chains_cp = new CF.list_cp CF.string_wrappers ~group ["chains"] []
    "The chains to apply"
  in
  { group ; in_cp ; chains_cp }
;;

let on_type_error file cp _ _ _ =
  let msg = Printf.sprintf "Bad value type for field %s"
    (String.concat "." cp#get_name)
  in
  error ~file msg
;;

let load config dir =
  let from_in_data =
    let data_dir = Config.data_dir config in
    Misc.path_under ~parent: data_dir dir
  in
  let file = Filename.concat dir input_basename in
  let g = mk_spec_group () in
  try
    g.group#read ~no_default: true
    ~on_type_error: (on_type_error file) file;
    let in_files = List.map
      (fun file -> (file, Misc.get_git_id (Filename.concat dir file)))
      g.in_cp#get
    in
    { dir = dir ;
      from_in_data = from_in_data ;
      in_files = in_files ;
      chains = g.chains_cp#get ;
    }
  with
    CF.Missing_cp cp ->
      let msg = Printf.sprintf "Missing field in %s"
        (String.concat "." cp#get_name)
      in
      error ~file msg
  | Stream.Error _ ->
      error ~file "Syntax error"
  | Sys_error s -> error ~file s
;;

let write dir =
  let file = Filename.concat dir input_basename in
  let g = mk_spec_group () in
  g.group#read ~on_type_error: (on_type_error file) file;
  g.group#write file
;;

let list_inputs config =
  let data_dir = Config.data_dir config in
  let pred s = (Filename.basename s) = input_basename in
  let spec_files = Find.find_list
    Find.Stderr [data_dir] [Find.Type Unix.S_REG ; Find.Predicate pred]
  in
  List.map
  (fun f -> Misc.path_under ~parent: data_dir (Filename.dirname f))
  spec_files
;;

