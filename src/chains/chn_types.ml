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

let iri_chain_module prefix modname =
  let s = String.concat "/" modname in
  Grdfs.iri_chain_module prefix s
;;

let iri_chain prefix fullname =
  let modname = String.concat "/" (chain_modname fullname) in
  let name = chain_basename fullname in
  Grdfs.iri_chain ~prefix ~modname name
;;

let is_iri_chain_module prefix iri =
  match Grdfs.is_iri_chain_module prefix iri with
    None -> None
  | Some slashes_modname ->
      Some (Misc.split_string slashes_modname ['/'])
;;

let is_iri_chain prefix iri =
  match Grdfs.is_iri_chain prefix iri with
    None -> None
  | Some (slashes_modname, name) ->
      Some (Misc.split_string slashes_modname ['/'], name)
;;

let iri_intf_of_interface_spec ~prefix s  =
  match Misc.split_string s ['/'] with
    [tool ; intf] ->
      let tool = Grdfs.iri_tool ~prefix ~tool in
      Grdfs.iri_intf ~tool ~intf
  | _ ->
      failwith (Printf.sprintf "invalid interface name: %S" s)
;;


type fchain_name = chain_name * version_id option
let fchain_id (_,id) = id;;
let fchain_chainname (name,_) = name;;
let fchain_modname (name,_) = chain_modname name;;
let fchain_basename (name,_) = chain_basename name;;
let mk_fchain_name name id = (name, Misc.opt_of_string id);;

let iri_fchain_module prefix modname =
  let s = String.concat "/" modname in
  Grdfs.iri_fchain_module prefix s
;;

let iri_fchain prefix (fullname, id) =
  let modname = String.concat "/" (chain_modname fullname) in
  let name = chain_basename fullname in
  Grdfs.iri_fchain ~prefix ~modname ?id ~name
;;

let is_iri_fchain_module prefix iri =
  (*prerr_endline (Printf.sprintf "is_iri_fchain_module %s" (Rdf_iri.string iri));*)
  match Grdfs.is_iri_fchain_module prefix iri with
    None -> None
  | Some slashes_modname ->
      let modname = Misc.split_string slashes_modname ['/'] in
      Some (modname)
;;

let is_iri_fchain ctx iri =
  let prefix = ctx.ctx_cfg.Config.rest_api in
  match Grdfs.is_iri_fchain prefix iri with
    None -> None
  | Some ((modname, name), id) ->
      match Grdfs.subject_iri ctx.ctx_rdf
        ~pred: Grdfs.genet_flattenedto ~obj: (Rdf_term.Iri iri)
      with
        None -> None
      | Some _ -> Some ((modname, name), Misc.opt_of_string id)
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

let iri_ichain_module prefix modname =
  let s = String.concat "/" modname in
  Grdfs.iri_ichain_module prefix s
;;

let iri_ichain prefix (fullname, id) =
  let modname = String.concat "/" (chain_modname fullname) in
  let name = chain_basename fullname in
  Grdfs.iri_ichain ~prefix ~modname ~name ~id
;;

let iri_inst_opn_of_flat_opn ?(ichain=false) ?cpt ~prefix ~inst ~flat =
  let base =
    match Grdfs.is_in_fchains prefix flat with
    | Some x -> x
    | None when not ichain ->
        failwith
        (Printf.sprintf "%S is not a flat chain iri" (Rdf_iri.string flat))
    | None ->
        match Grdfs.is_in_ichains prefix flat with
         None ->
            failwith
            (Printf.sprintf "%S is not an inst chain iri" (Rdf_iri.string flat))
        | Some x -> x
  in
  let inst =
    match Grdfs.split_fchain_name base with
      `Fchain_op (_,_,path) -> List.fold_left Rdf_iri.concat inst path
    | `Fullname _ -> inst
    | _ ->
        let msg = Printf.sprintf
          "%S is not a flat chain iri" (Rdf_iri.string flat)
        in
        failwith msg
  in
  match cpt with
    None -> inst
  | Some n ->
      let name = match List.rev (Rdf_iri.path inst) with [] -> assert false | name :: _ -> name in
      Rdf_iri.concat (Grdfs.iri_parent inst) (Printf.sprintf "%s-%d" name n)
;;

let iri_inst_port_of_flat_port ?ichain ?cpt ctx ~inst ~flat =
  let flat_opn = Grdfs.port_container flat in
  let inst_opn = iri_inst_opn_of_flat_opn ?ichain ?cpt
    ~prefix: ctx.ctx_cfg.Config.rest_api ~inst ~flat: flat_opn in
  let dir = Grdf_port.port_dir flat in
  let rank = Grdf_port.port_rank flat in
  let inst_port =
    let f = match dir with
        Grdf_port.In -> Grdfs.iri_intf_in_port
      | Grdf_port.Out -> Grdfs.iri_intf_out_port
    in
    f inst_opn rank
  in
  inst_port
;;

let is_iri_ichain prefix iri =
  match Grdfs.is_iri_ichain prefix iri with
    None -> None
  | Some ((modname, name), id) -> Some ((modname, name), id)
;;

let ichain_op_name ~ichain op =
  let parent = String.concat "/" (Rdf_iri.path ichain) in
  let path = String.concat "/" (Rdf_iri.path op) in
  Misc.path_under ~parent path
;;

let iri_ichain_op ichain path =
  List.fold_left Rdf_iri.concat ichain path
;;


