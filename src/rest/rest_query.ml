(** *)

open Rdf_node;;
open Rest_types;;

exception Not_implemented of string;;
let not_implemented msg = raise (Not_implemented msg);;

let rest_api_path config =
  let path = Neturl.join_path (Rdf_uri.path config.Config.rest_api) in
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
     (Rdf_uri.concat config.Config.rest_api f, (f, t)))
  ["style.css", "text/css" ; "genet-logo.svg", "image/svg+xml"]
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


let uri_of_query_path ctx path =
  let url = Rdf_uri.neturl ctx.ctx_cfg.Config.rest_api in
  let url = Neturl.modify_url ~path: [] url in
  let s = Neturl.string_of_url url in
  let uri = s ^ path in
  (*prerr_endline (Printf.sprintf "uri_of_query_path: path=%s => uri=%s" path uri);*)
  Rdf_uri.uri uri
;;

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
  let prefix = ctx.ctx_cfg.Config.rest_api in
  let prefix_path = String.concat "/" (Rdf_uri.path prefix) in
  Neturl.split_path (Misc.path_under ~parent: prefix_path path)
;;

let uri_append uri s =
  let s_uri = Rdf_uri.string uri in
  let len = String.length s_uri in
  let s_uri = if s_uri.[len-1] = '/' then String.sub s_uri 0 (len-1) else s_uri in
  let s_uri = Printf.sprintf "%s/%s" s_uri s in
  prerr_endline (Printf.sprintf "uri_append => %s" s_uri);
  Rdf_uri.uri s_uri
;;

let rec read_path_branch uri_parent = function
  [] -> assert false
| s :: q ->
    let uri = uri_append uri_parent s in
    match q with
      [] | [""] -> Branch uri
    | _ -> read_path_branch uri q
;;

let read_path_branches uri_parent cur_uri = function
  [] | [""] -> Branches uri_parent
| q -> read_path_branch cur_uri q
;;

let read_path_intfs uri cur_uri = function
  [] | [""] -> Intfs uri
| [s] -> Intf (uri_append cur_uri s)
| _ -> Other uri
;;

let read_path_versions uri cur_uri = function
  [] | [""] -> Versions uri
| [s] -> Version (uri_append cur_uri s)
| _ -> Tool uri
;;

let read_path_tools =
  let next =
    [
      Grdfs.suffix_branches, read_path_branches ;
      Grdfs.suffix_intfs, read_path_intfs ;
      Grdfs.suffix_versions, read_path_versions ;
    ]
  in
  fun _ uri -> function
    | [] | [""] -> Tools
    | [tool] -> Tool (uri_append uri tool)
    | tool :: s :: q ->
        let uri_tool = uri_append uri tool in
        try (List.assoc s next) uri_tool (uri_append uri_tool s) q
        with Not_found -> Tool uri
;;

let rec read_path_chain uri modpath = function
  [] | [""] ->
    Chain_module
     (Chn_types.chain_modname_of_string (String.concat "." (List.rev modpath)))
| m :: q when String.capitalize m = m ->
    read_path_chain uri (m :: modpath) q
| basename :: _ ->
    let name = String.concat "." (List.rev (basename :: modpath)) in
    let chain_name = Chn_types.chain_name_of_string name in
    Chain chain_name
;;

let read_path_chains _ uri = function
  [] | [""] -> Chains
| modname :: q ->
    read_path_chain uri [modname] q
;;

let rec read_path_fchain ctx uri modpath = function
  [] | [""] ->
    Flat_chain_module
     (Chn_types.chain_modname_of_string (String.concat "." (List.rev modpath)))
| m :: q when String.capitalize m = m ->
    read_path_fchain ctx uri (m :: modpath) q
| basename :: q ->
    let name = String.concat "." (List.rev (basename :: modpath)) in
    let chain_name = Chn_types.chain_name_of_string name in
    match q with
      [] ->
        let fchain_name = Chn_types.mk_fchain_name chain_name "" in
        Flat_chain_list fchain_name
    | id :: _ ->
        let fchain_name = Chn_types.mk_fchain_name chain_name id in
        let uri = Chn_types.uri_fchain ctx.ctx_cfg.Config.rest_api fchain_name in
        Flat_chain uri
;;

let read_path_fchains ctx uri = function
  [] | [""] -> Flat_chains
| modname :: q ->
    read_path_fchain ctx uri [modname] q
;;

let rec read_path_ichain ctx uri modpath = function
  [] | [""] -> (* TODO: define and handle Inst_chain_module *)
    Flat_chain_module
     (Chn_types.chain_modname_of_string (String.concat "." (List.rev modpath)))
| m :: q when String.capitalize m = m ->
    read_path_ichain ctx uri (m :: modpath) q
| basename :: q ->
    let name = String.concat "." (List.rev (basename :: modpath)) in
    let chain_name = Chn_types.chain_name_of_string name in
    match q with
      [] -> (* TODO: define and handle Inst_chain_list *)
        let fchain_name = Chn_types.mk_fchain_name chain_name "" in
        Flat_chain_list fchain_name
    | id :: _ ->
        let ichain_name = Chn_types.mk_ichain_name chain_name id in
        let uri = Chn_types.uri_ichain ctx.ctx_cfg.Config.rest_api ichain_name in
        Inst_chain uri
;;

let read_path_ichains ctx uri = function
  [] | [""] -> Chains (* TODO: define and handle Inst_chains *)
| modname :: q ->
    read_path_ichain ctx uri [modname] q
;;

let read_path_filetypes ctx uri = function
  [] | [""] -> Filetypes
| s :: _ -> Filetype (uri_append uri s)
;;

let rec read_path_out_path ?(raw=false) outpath = function
  [] -> Out_file (List.rev outpath, raw)
| s :: q -> read_path_out_path ~raw (s :: outpath) q
;;

let read_path_out ctx uri = function
  [] -> (* TODO: define and handle Outfiles *) Tools
| "raw" :: s :: q -> read_path_out_path ~raw: true [s] q
| s :: q -> read_path_out_path [s] q
;;

let read_path =
  let next =
    [ Grdfs.suffix_tools, read_path_tools ;
      Grdfs.suffix_chains, read_path_chains ;
      Grdfs.suffix_fchains, read_path_fchains ;
      Grdfs.suffix_ichains, read_path_ichains ;
      Grdfs.suffix_filetypes, read_path_filetypes ;
      Grdfs.suffix_out, read_path_out ;
    ]
  in
  fun ctx uri -> function
    | [] | [""] -> Tools
    | s :: q ->
        try (List.assoc s next) ctx (uri_append ctx.ctx_cfg.Config.rest_api s) q
        with Not_found ->
            try
              let static_files = allowed_files ctx.ctx_cfg in
              let (f, t) = List.assoc uri static_files in
              let f = List.fold_left Filename.concat ctx.ctx_cfg.Config.root_dir
                ["in" ; "web" ; f]
              in
              Static_file (f, t)
            with
              Not_found -> Other uri
;;


let rec thing_of_path ctx path =
  (* we must change the path to an uri according to rest_api *)
  let uri = uri_of_query_path ctx path in
  let app_path = application_path ctx path in
  read_path ctx uri app_path
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
