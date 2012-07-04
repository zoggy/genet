(** *)

type user = string
type version_id = string

type context =
  { ctx_rdf: Grdf_types.world ;
    ctx_cfg: Config.t ;
    ctx_user : user option ;
  }

type chain_modname = string list
type chain_basename = string
type chain_name = chain_modname * chain_basename

let chain_modname (modname,_) = modname
let chain_basename (_,name) = name
let mk_chain_name modname name = (modname, name)

let string_of_chain_modname = String.concat ".";;
let string_of_chain_basename s = s;;
let string_of_chain_name (modname, name) =
  Printf.sprintf "%s.%s" (String.concat "." modname) name
;;

let chain_modname_of_string s =
  List.map Misc.strip_string (Misc.split_string s ['.']);;
let chain_basename_of_string s = s;;

let chain_name_of_string s =
  match List.rev (Misc.split_string s ['.']) with
    [] | [_] -> failwith ("Invalid chain full name: "^s)
  | name :: rev_modname -> (List.rev rev_modname, name)
;;

let compare_chain_modname = Pervasives.compare;;
let compare_chain_basename = Pervasives.compare;;
let compare_chain_name = Pervasives.compare;;

let uri_chain_module prefix modname =
  let s = String.concat "/" modname in
  Grdfs.uri_chain_module prefix s
;;

let uri_chain prefix fullname =
  let modname = String.concat "/" (chain_modname fullname) in
  let name = chain_basename fullname in
  Grdfs.uri_chain ~prefix ~modname name
;;

let is_uri_chain_module prefix uri =
  match Grdfs.is_uri_chain_module prefix uri with
    None -> None
  | Some slashes_modname ->
      Some (Misc.split_string slashes_modname ['/'])
;;

let is_uri_chain prefix uri =
  match Grdfs.is_uri_chain prefix uri with
    None -> None
  | Some (slashes_modname, name) ->
      Some (Misc.split_string slashes_modname ['/'], name)
;;

let uri_intf_of_interface_spec ~prefix s  =
  match Misc.split_string s ['/'] with
    [tool ; intf] ->
      let tool = Grdfs.uri_tool ~prefix ~tool in
      Grdfs.uri_intf ~tool ~intf
  | _ ->
      failwith (Printf.sprintf "invalid interface name: %S" s)
;;


type fchain_name = chain_name * version_id option
let fchain_id (_,id) = id;;
let fchain_chainname (name,_) = name;;
let fchain_modname (name,_) = chain_modname name;;
let fchain_basename (name,_) = chain_basename name;;
let mk_fchain_name name id = (name, Misc.opt_of_string id);;

let uri_fchain_module prefix modname =
  let s = String.concat "/" modname in
  Grdfs.uri_fchain_module prefix s
;;

let uri_fchain prefix (fullname, id) =
  let modname = String.concat "/" (chain_modname fullname) in
  let name = chain_basename fullname in
  Grdfs.uri_fchain ~prefix ~modname ?id ~name
;;

let is_uri_fchain_module prefix uri =
  prerr_endline (Printf.sprintf "is_uri_fchain_module %s" (Rdf_uri.string uri));
  match Grdfs.is_uri_fchain_module prefix uri with
    None -> None
  | Some slashes_modname ->
      let modname = Misc.split_string slashes_modname ['/'] in
      Some (modname)
;;

let is_uri_fchain prefix uri =
  match Grdfs.is_uri_fchain prefix uri with
    None -> None
  | Some ((modname, name), id) ->
      Some ((modname, name), Misc.opt_of_string id)
;;

type ichain_name = chain_name * string
let ichain_id (_,id) = id;;
let ichain_chainname (name,_) = name;;
let ichain_modname (name,_) = chain_modname name;;
let ichain_basename (name,_) = chain_basename name;;
let mk_ichain_name name id = (name, id);;

let string_of_ichain_name (chain_name, id) =
  Printf.sprintf "%s/%s" (string_of_chain_name chain_name) id
;;

let uri_ichain_module prefix modname =
  let s = String.concat "/" modname in
  Grdfs.uri_ichain_module prefix s
;;

let uri_ichain prefix (fullname, id) =
  let modname = String.concat "/" (chain_modname fullname) in
  let name = chain_basename fullname in
  Grdfs.uri_ichain ~prefix ~modname ~name ~id
;;

let uri_inst_opn_of_flat_opn ~prefix ~inst ~flat =
  match Grdfs.is_in_fchains prefix flat with
    None ->
      failwith
      (Printf.sprintf "%S is not a flat chain uri" (Rdf_uri.string flat))
  | Some base ->
      match Grdfs.split_fchain_name base with
        `Fchain_op (_,_,path) -> List.fold_left Rdf_uri.concat inst path
      | `Fullname _ -> inst
      | _ ->
          let msg = Printf.sprintf
            "%S is not a flat chain uri" (Rdf_uri.string flat)
          in
          failwith msg
;;

let uri_inst_port_of_flat_port ctx ~inst ~flat =
  let flat_opn = Grdfs.port_container flat in
  let inst_opn = uri_inst_opn_of_flat_opn
    ~prefix: ctx.ctx_cfg.Config.rest_api ~inst ~flat: flat_opn in
  let dir = Grdf_port.port_dir flat in
  let rank = Grdf_port.port_rank flat in
  let inst_port =
    let f = match dir with
        Grdf_port.In -> Grdfs.uri_intf_in_port
      | Grdf_port.Out -> Grdfs.uri_intf_out_port
    in
    f inst_opn rank
  in
  inst_port
;;

let is_uri_ichain prefix uri =
  match Grdfs.is_uri_ichain prefix uri with
    None -> None
  | Some ((modname, name), id) -> Some ((modname, name), id)
;;
(*
let is_uri_ichain_module prefix uri =
  prerr_endline (Printf.sprintf "is_uri_ichain_module %s" (Rdf_uri.string uri));
  match Grdfs.is_uri_ichain_module prefix uri with
    None -> None
  | Some slashes_modname ->
      let modname = Misc.split_string slashes_modname ['/'] in
      Some (modname)
;;

let is_uri_ichain prefix uri =
  match Grdfs.is_uri_ichain prefix uri with
    None -> None
  | Some ((modname, name), id) ->
      Some ((modname, name), Misc.opt_of_string id)
;;
*)