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


      (embed1
         (fun _ uri -> Chn_types.is_uri_fchain_module ctx.ctx_cfg.Config.rest_api uri)
         (fun modname -> Flat_chain_module modname)) ;

      (fun uri ->
        match Chn_types.is_uri_fchain ctx.ctx_cfg.Config.rest_api uri with
           None -> None
         | Some (fullname, id) -> Some (Flat_chain (fullname, id))) ;
    ]
  in
  let rec iter = function
    [] -> None
  | f :: q ->
      match f uri with
        None -> iter q
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
  | Some c when c =@= Grdfs.genet_tool -> Tool uri
  | Some c when c =@= Grdfs.genet_branch -> Branch uri
  | Some c when c =@= Grdfs.genet_version -> Version uri
  | Some c when c =@= Grdfs.genet_intf -> Intf uri
  | Some c when c =@= Grdfs.genet_filetype -> Filetype uri
  | Some c -> prerr_endline (Rdf_uri.string c); Other uri
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
