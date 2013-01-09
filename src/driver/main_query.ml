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

(** Main module of the genet-query program.*)

open Grdf_tool;;

type mode =
  | Tools
  | Branches
  | Versions
  | Interfaces
  | Filetypes
  | Dot
  | Test of int
  | File of string
  | Inst of string

let mode = ref None;;

let options =
  Options.option_version "Genet-query" ::
  Options.option_config ::
  ("--tools", Arg.Unit (fun () -> mode := Some Tools), " print tools") ::

  ("--branches", Arg.Unit (fun () -> mode := Some Branches),
   " print branches (all or only the ones of the given parents)") ::

  ("--versions", Arg.Unit (fun () -> mode := Some Versions),
   " print versions (all or only the ones of the given tools or branches)") ::

  ("--interfaces", Arg.Unit (fun () -> mode := Some Interfaces),
   " print interfaces (all or only the ones of the given tools or branches)") ::

  ("--filetypes", Arg.Unit (fun () -> mode := Some Filetypes),
   " print filetypes") ::

  ("--file", Arg.String (fun s -> mode := Some (File s)),
   "<url> print the filename corresponding to the given file url") ::

  ("--inst", Arg.String (fun s -> mode := Some (Inst s)),
   "<url> look for other executions 'almost' like the given one") ::

  ("--dot", Arg.Unit (fun () -> mode := Some Dot),
   " print graph in graphviz format") ::

  ("--test", Arg.Int (fun n -> mode := Some (Test n)),
   " not documented (testing purpose)") ::

  []
;;

let pruri_endline uri = print_endline (Rdf_uri.string uri);;

let list_tools wld =
  let tools = Grdf_tool.tools wld in
  List.iter pruri_endline tools
;;

let list_branches wld options =
  let branches =
    match options.Options.args with
      [] -> List.map (fun b -> b.Grdf_branch.bch_uri) (Grdf_branch.branches wld)
    | l ->
        let add set elt = Uriset.add elt set in
        let f set s_uri =
          List.fold_left add set (Grdf_branch.subs wld (Rdf_uri.uri s_uri))
        in
        let set = List.fold_left f Uriset.empty l in
        Uriset.elements set
  in
  List.iter pruri_endline branches
;;

let list_versions wld options =
  let versions =
    match options.Options.args with
      [] -> Grdf_version.versions wld
    | l ->
        let add set elt = Uriset.add elt set in
        let f set s_uri =
          List.fold_left add set
          (Grdf_version.versions_of ~recur: true wld (Rdf_uri.uri s_uri))
        in
        let set = List.fold_left f Uriset.empty l in
        Uriset.elements set
  in
  List.iter pruri_endline versions
;;

let list_interfaces wld options =
  let intfs =
    match options.Options.args with
      [] -> Grdf_intf.intfs wld
    | l ->
        let f set s_uri =
          Uriset.union set
          (Grdf_intf.intfs_of ~recur: true wld (Rdf_uri.uri s_uri))
        in
        List.fold_left f Uriset.empty l
  in
  Uriset.iter (fun uri -> print_endline (Grdf_intf.string_of_intf wld uri)) intfs
;;

let list_filetypes wld =
  let l = Grdf_ftype.filetypes wld in
  List.iter (fun ft -> prerr_endline (Grdf_ftype.string_of_filetype wld ft)) l
;;

let print_filename_of_url config s =
  let prefix = config.Config.rest_api in
  let out_pref = Grdfs.uri_outfile_path prefix [] in
  let out_pref_raw = Grdfs.uri_outfile_path ~raw: true prefix [] in
  let in_pref = Grdfs.uri_input_path prefix [] in
  let in_pref_raw = Grdfs.uri_input_path ~raw: true prefix  [] in
  let out_dir = Config.out_dir config in
  let in_dir = Config.data_dir config in
  let prefixes = (* order matters as raw prefixes are prefix of "not raw" *)
    [ out_pref_raw, out_dir ; out_pref, out_dir ;
      in_pref_raw, in_dir ; in_pref, in_dir ;
    ]
  in
  let rec f = function
    [] -> raise Not_found
  | (prefix, dir) :: q ->
      try
        let path = Misc.path_under ~parent: (Rdf_uri.string prefix) s in
        Filename.concat dir path
      with _ -> f q
  in
  try print_endline (f prefixes)
  with
    Not_found ->
      failwith (Printf.sprintf "%S does not correspond to an input or output file url" s)
;;

let lookup_insts ctx s =
  let _uri_inst = Rdf_uri.uri s in
  []
;;

let lookup_and_print_insts ctx s =
  let insts = lookup_insts ctx s in
  List.iter
    (fun uri -> print_endline (Rdf_uri.string uri))
    insts;
  let dot = Chn_lookup.make_graph ctx (Rdf_uri.uri s) in
  let svg = Grdf_dot.dot_to_svg ~program: "circo" dot in
  print_endline (Xtmpl.string_of_xmls svg)
;;

let dot wld = print_endline (Grdf_dot.dot wld);;

let test wld config n =
  match n with
  | 2 ->
      begin
        let chain_files = Chn_io.chain_files config in
        List.iter prerr_endline chain_files
      end
  | _  ->
      begin
        let intfs = Grdf_intf.intfs wld in
        if Uriset.is_empty intfs then
          failwith "no interfaces to use to test";

        match Grdf_ftype.filetypes wld with
          [] -> failwith "no filetype to use to test"
        | filetypes ->
            let filetypes = Array.of_list filetypes in
            prerr_endline (Printf.sprintf "%d filetypes" (Array.length filetypes));
            let n = Array.length filetypes in
            (*          Grdf_intf.delete_ports wld Grdf_intf.In intf;*)

            Random.self_init();
            let dir = if Random.int 2 = 0 then Grdf_port.In else Grdf_port.Out in
            let ftype = Grdf_ftype.name wld filetypes.(Random.int n) in
            let port =
              if Random.int 1 = 0 then Grdf_types.Set (Grdf_types.T ftype) else Grdf_types.T ftype
            in
            let _intfs = Uriset.elements intfs in

            let intf = Rdf_uri.uri
             "http://localhost:8082/tools/altergo/interfaces/ae-prove"
            (*  List.nth intfs (Random.int (List.length intfs))*)
            in

            prerr_endline (Grdf_intf.string_of_intf wld intf);
            Grdf_port.add_port wld intf dir port;
            prerr_endline (Grdf_intf.string_of_intf wld intf);

(*            Grdf_port.delete_ports wld intf Grdf_port.In ;*)
            prerr_endline (Grdf_intf.string_of_intf wld intf);
      end
;;

let main () =
  let opts = Options.parse options in
  let config = Config.read_config opts.Options.config_file in
  let rdf_wld = Grdf_init.open_graph config in
  begin
    match !mode with
      None -> ()
    | Some Tools -> list_tools rdf_wld
    | Some Branches -> list_branches rdf_wld opts
    | Some Versions -> list_versions rdf_wld opts
    | Some Interfaces -> list_interfaces rdf_wld opts
    | Some Filetypes -> list_filetypes rdf_wld
    | Some Dot -> dot rdf_wld
    | Some (Test n) -> test rdf_wld config n
    | Some (File s) -> print_filename_of_url config s
    | Some (Inst s) ->
        lookup_and_print_insts
        { Chn_types.ctx_rdf = rdf_wld ; ctx_cfg = config ; ctx_user = None}
        s
  end
;;

let () = Misc.safe_main main;;
