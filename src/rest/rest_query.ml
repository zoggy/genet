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

open Rdf_term;;
open Rest_types;;

exception Not_implemented of string;;
let not_implemented msg = raise (Not_implemented msg);;

let rest_api_path config =
  let path = Neturl.join_path (Rdf_iri.path config.Config.rest_api) in
  let path =
    let len = String.length path in
    if len <= 0 || path.[len-1] <> '/' then
      path ^ "/"
    else
      path
  in
  path
;;

let allowed_files config =
  List.map
  (fun (f, t) ->
     (Rdf_iri.string (Rdf_iri.concat config.Config.rest_api f), (f, t)))
  [ "style.css", "text/css" ;
    "genet-logo.svg", "image/svg+xml" ;
    "star.svg", "image/svg+xml" ;
  ]
;;


let xhtml_handlers =
  { h_get = Rest_xhtml.get ;
    h_del = (fun _ _ -> not_implemented "xhtml delete");
    h_post = (fun _ _ -> not_implemented "xhtml post");
    h_put = (fun _ _ -> not_implemented "xhtml put");
  }

let json_handlers =
  { h_get = (fun _ _ _ -> not_implemented "json get") ;
    h_del = (fun _ _ -> not_implemented "json delete");
    h_post = (fun _ _ -> not_implemented "json post");
    h_put = (fun _ _ -> not_implemented "json put");
  }


(*
let uri_of_query_path ctx path =
  let url = Rdf_uri.neturl ctx.ctx_cfg.Config.rest_api in
  let url = Neturl.modify_url ~path: [] url in
  let s = Neturl.string_of_url url in
  let uri = s ^ path in
  (*prerr_endline (Printf.sprintf "uri_of_query_path: path=%s => uri=%s" path uri);*)
  Rdf_uri.uri uri
;;
*)

(*
let try_is_uri_of ctx uri =
  let embed1 test build =
    fun uri ->
      match test ctx.ctx_rdf uri with
        None -> None
      | Some uri -> Some (build uri)
  in
  let l =
    [
      (embed1 Grdfs.is_uri_tool_versions (fun uri -> Versions uri)) ;
      (embed1 Grdfs.is_uri_tool_branches (fun uri -> Branches uri)) ;
      (embed1 Grdfs.is_uri_tool_interfaces (fun uri -> Intfs uri)) ;

      (embed1 Grdfs.is_uri_branch_versions (fun uri -> Versions uri)) ;
      (embed1 Grdfs.is_uri_branch_branches (fun uri -> Branches uri)) ;
      (embed1 Grdfs.is_uri_branch_interfaces (fun uri -> Intfs uri)) ;

      (embed1 Grdfs.is_uri_version_interfaces (fun uri -> Intfs uri)) ;

      (embed1
         (fun _ uri -> Chn_types.is_uri_chain_module ctx.ctx_cfg.Config.rest_api uri)
         (fun modname -> Chain_module (Chn_types.chain_modname_of_string modname))) ;

      (embed1
         (fun _ uri -> Chn_types.is_uri_chain ctx.ctx_cfg.Config.rest_api uri)
         (fun fullname -> Chain (Chn_types.chain_name_of_string fullname))) ;

      (fun uri ->
        match Chn_types.is_uri_fchain ctx.ctx_cfg.Config.rest_api uri with
           None -> None
         | Some fchain_name ->
            let t =
              match Chn_types.fchain_id fchain_name with
                 None -> Flat_chain_list fchain_name
               | Some id -> Flat_chain uri
             in
             Some t
      );

      (embed1
         (fun _ uri -> Chn_types.is_uri_fchain_module ctx.ctx_cfg.Config.rest_api uri)
         (fun modname -> Flat_chain_module modname)) ;

    ]
  in
  let rec iter = function
    [] -> None
  | f :: q ->
      match f uri with
        None ->
          prerr_endline (Printf.sprintf "f %S fail" (Rdf_uri.string uri));
           iter q
      | x -> x
  in
  iter l
;;

let (=@=) = Rdf_uri.equal;;

let rec thing_of_path ctx path =
  (* we must change the path to an uri according to rest_api *)
  let uri = uri_of_query_path ctx path in
  let wld = ctx.ctx_rdf in
  let static_files = allowed_files ctx.ctx_cfg in
  prerr_endline "allowed files:";
  List.iter (fun (f,_) -> prerr_endline (Rdf_uri.string f)) static_files;
  match Grdfs.class_of wld (Uri uri) with
  | None when uri =@= Grdfs.uri_tools ctx.ctx_rdf.Grdf_types.wld_prefix -> Tools
  | None when uri =@= Grdfs.uri_filetypes ctx.ctx_rdf.Grdf_types.wld_prefix -> Filetypes
  | None when uri =@= Grdfs.uri_chains ctx.ctx_rdf.Grdf_types.wld_prefix -> Chains
  | None when uri =@= Grdfs.uri_fchains ctx.ctx_rdf.Grdf_types.wld_prefix -> Flat_chains
  | Some c when c =@= Grdfs.genet_tool -> Tool uri
  | Some c when c =@= Grdfs.genet_branch -> Branch uri
  | Some c when c =@= Grdfs.genet_version -> Version uri
  | Some c when c =@= Grdfs.genet_intf -> Intf uri
  | Some c when c =@= Grdfs.genet_filetype -> Filetype uri
  | Some c when c =@= Grdfs.genet_flatchain -> Flat_chain uri
  | Some c when c =@= Grdfs.genet_instchain -> Inst_chain uri
  | None ->
      begin
        match try_is_uri_of ctx uri with
          Some res -> res
        | None ->
            try
              prerr_endline ("not a special uri: "^(Rdf_uri.string uri));
              let (f, t) = List.assoc uri static_files in
              let f = List.fold_left Filename.concat ctx.ctx_cfg.Config.root_dir
                ["in" ; "web" ; f]
              in
              Static_file (f, t)
            with
              Not_found -> Other uri
      end
  | Some c -> prerr_endline (Rdf_uri.string c); Other uri
;;
*)

(** Return the path relative to the application, i.e. the path to
  analyse to know what to do and return. *)
let application_path ctx path =
  let prefix = Rdf_iri.to_uri ctx.ctx_cfg.Config.rest_api in
  let prefix_path = String.concat "/" (Rdf_uri.path prefix) in
  Neturl.split_path (Misc.path_under ~parent: prefix_path path)
;;

(*
let uri_append uri s =
  let s_uri = Rdf_uri.string uri in
  let len = String.length s_uri in
  let s_uri = if s_uri.[len-1] = '/' then String.sub s_uri 0 (len-1) else s_uri in
  let s_uri = Printf.sprintf "%s/%s" s_uri s in
  prerr_endline (Printf.sprintf "uri_append => %s" s_uri);
  Rdf_uri.uri s_uri
;;
*)

let rec read_path_branch iri_parent = function
  [] -> assert false
| s :: q ->
    let iri = Rdf_iri.concat iri_parent s in
    match q with
      [] | [""] -> Branch iri
    | _ -> read_path_branch iri q
;;

let read_path_branches iri_parent cur_iri = function
  [] | [""] -> Branches iri_parent
| q -> read_path_branch cur_iri q
;;

let read_path_intfs iri cur_iri = function
  [] | [""] -> Intfs iri
| [s] -> Intf (Rdf_iri.concat cur_iri s)
| _ -> Other iri
;;

let read_path_versions iri cur_iri = function
  [] | [""] -> Versions iri
| [s] -> Version (Rdf_iri.concat cur_iri s)
| _ -> Tool iri
;;

let read_path_tools =
  let next =
    [
      Grdfs.suffix_branches, read_path_branches ;
      Grdfs.suffix_intfs, read_path_intfs ;
      Grdfs.suffix_versions, read_path_versions ;
    ]
  in
  fun _ iri -> function
    | [] | [""] -> Tools
    | [tool] -> Tool (Rdf_iri.concat iri tool)
    | tool :: s :: q ->
        let iri_tool = Rdf_iri.concat iri tool in
        try (List.assoc s next) iri_tool (Rdf_iri.concat iri_tool s) q
        with Not_found -> Tool iri
;;

let rec read_path_chain iri modpath = function
  [] | [""] ->
    Chain_module
     (Chn_types.chain_modname_of_string (String.concat "." (List.rev modpath)))
| m :: q when String.capitalize m = m ->
    read_path_chain iri (m :: modpath) q
| basename :: _ ->
    let name = String.concat "." (List.rev (basename :: modpath)) in
    let chain_name = Chn_types.chain_name_of_string name in
    Chain chain_name
;;

let read_path_chains _ iri = function
  [] | [""] -> Chains
| modname :: q ->
    read_path_chain iri [modname] q
;;

let rec read_path_fchain ctx iri modpath = function
  [] | [""] ->
    Flat_chain_module
     (Chn_types.chain_modname_of_string (String.concat "." (List.rev modpath)))
| m :: q when String.capitalize m = m ->
    read_path_fchain ctx iri (m :: modpath) q
| basename :: q ->
    let name = String.concat "." (List.rev (basename :: modpath)) in
    let chain_name = Chn_types.chain_name_of_string name in
    match q with
      [] ->
        let fchain_name = Chn_types.mk_fchain_name chain_name "" in
        Flat_chain_list fchain_name
    | id :: _ ->
        let fchain_name = Chn_types.mk_fchain_name chain_name id in
        let iri = Chn_types.iri_fchain ctx.ctx_cfg.Config.rest_api fchain_name in
        Flat_chain iri
;;

let read_path_fchains ctx iri = function
  [] | [""] -> Flat_chains
| modname :: q ->
    read_path_fchain ctx iri [modname] q
;;

let rec read_path_ichain ctx iri modpath = function
  [] | [""] -> (* TODO: define and handle Inst_chain_module *)
    Flat_chain_module
     (Chn_types.chain_modname_of_string (String.concat "." (List.rev modpath)))
| m :: q when String.capitalize m = m ->
    read_path_ichain ctx iri (m :: modpath) q
| basename :: q ->
    let name = String.concat "." (List.rev (basename :: modpath)) in
    let chain_name = Chn_types.chain_name_of_string name in
    match q with
      [] -> (* TODO: define and handle Inst_chain_list *)
        let fchain_name = Chn_types.mk_fchain_name chain_name "" in
        Flat_chain_list fchain_name
    | id :: q ->
        let ichain_name = Chn_types.mk_ichain_name chain_name id in
        let iri = Chn_types.iri_ichain ctx.ctx_cfg.Config.rest_api ichain_name in
        match q with
          [] -> Inst_chain iri
        | path ->
            let iri_op = Chn_types.iri_ichain_op iri path in
            Inst_chain_op iri_op
;;

let read_path_ichains ctx iri = function
  [] | [""] -> Inst_chains
| s :: path when s = Grdfs.suffix_producers_of -> Inst_producers_of path
| modname :: q ->
    read_path_ichain ctx iri [modname] q
;;

let read_path_filetypes ctx iri = function
  [] | [""] -> Filetypes
| s :: _ -> Filetype (Rdf_iri.concat iri s)
;;

let rec read_path_out_path ?(raw=false) outpath = function
  [] -> Out_file (List.rev outpath, raw)
| s :: q -> read_path_out_path ~raw (s :: outpath) q
;;

let read_path_out ctx iri = function
  [] -> (* TODO: define and handle Outfiles *) Inst_chains
| "raw" :: s :: q -> read_path_out_path ~raw: true [s] q
| s :: q -> read_path_out_path [s] q
;;

let rec read_path_in_path ctx raw inpath = function
  [] -> Input (List.rev inpath)
| s :: q ->
    let data_dir = Config.data_dir ctx.ctx_cfg in
    let dir = List.fold_left Fname.concat_s data_dir (List.rev (s :: inpath)) in
    let spec_file = Fname.concat_s dir Ind_io.input_basename in
    prerr_endline (Printf.sprintf "spec_file=%S" (Fname.abs_string spec_file));
    if Sys.file_exists (Fname.abs_string spec_file) then
      begin
        match q with
          [] -> Input (List.rev (s :: inpath))
        | _ -> Input_file (inpath, s::q, raw)
      end
    else
      read_path_in_path ctx raw (s :: inpath) q
;;

let read_path_in ctx iri = function
  [] -> Inputs
| "raw" :: path -> read_path_in_path ctx true [] path
| path -> read_path_in_path ctx false [] path
;;

let read_path_diff ctx iri = function
| h :: _ when h = Grdfs.suffix_ichains ->
    Diff_inst_chains
| _ -> Diff_inst_chains (* FIXME: show a menu for diffs ? *)
;;

let read_path =
  let next =
    [ Grdfs.suffix_tools, read_path_tools ;
      Grdfs.suffix_chains, read_path_chains ;
      Grdfs.suffix_fchains, read_path_fchains ;
      Grdfs.suffix_ichains, read_path_ichains ;
      Grdfs.suffix_filetypes, read_path_filetypes ;
      Grdfs.suffix_out, read_path_out ;
      Grdfs.suffix_in, read_path_in ;
      Grdfs.suffix_diff, read_path_diff ;
    ]
  in
  fun ctx -> function
    | [] | [""] -> Tools
    | s :: q ->
        try (List.assoc s next) ctx (Rdf_iri.concat ctx.ctx_cfg.Config.rest_api s) q
        with Not_found ->
            let iri = List.fold_left Rdf_iri.concat
              ctx.ctx_cfg.Config.rest_api
                (s :: q)
            in
            try
              let static_files = allowed_files ctx.ctx_cfg in
              let (f, t) = List.assoc (Rdf_iri.string iri) static_files in
              let f = List.fold_left Fname.concat_s ctx.ctx_cfg.Config.root_dir
                ["in" ; "web" ; f]
              in
              Static_file (Fname.abs_string f, t)
            with
              Not_found ->
                Other iri
;;


let rec thing_of_path ctx path =
  let app_path = application_path ctx path in
  read_path ctx app_path
;;

let handler_by_method h ctx = function
  Get (path, args) -> h.h_get ctx (thing_of_path ctx path) args
| Delete path -> h.h_del ctx (thing_of_path ctx path)
| Post (path, json) -> h.h_post ctx (thing_of_path ctx path) json
| Put (path, json) -> h.h_put ctx (thing_of_path ctx path) json

let query content_type =
  match content_type with
    Xhtml -> handler_by_method xhtml_handlers
  | Json -> handler_by_method json_handlers
;;
