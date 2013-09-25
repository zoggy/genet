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

(** Replying to xhtml queries. *)

open Rdf_term;;
open Rest_types;;

module Iriset = Rdf_iri.Iriset
module Irimap = Rdf_iri.Irimap

let svg_width = 700;;
let svg_height = 200;;
let svg_dpi = 96. ;;

let dot_to_svg ?(svg_w=svg_width) ?(svg_h=svg_height) dot =
  let size = String.length dot in
  if size > 50_000 then
    [Xtmpl.E (("", "div"), [("", "class"), "alert alert-warning"],
      [Xtmpl.D "Dot graph would have been too big; it was not generated"])]
  else
    begin
      let w = (float svg_w) /. svg_dpi in
      let h = (float svg_h) /. svg_dpi in
      let options = Printf.sprintf "-Gfontsize=8. -Gdpi=%.2f -Gsize=\"%.0f,%.0f!\"" svg_dpi w h in
      try
        Grdf_dot.dot_to_svg ~options ~size: (svg_w,svg_h) dot
      with
        Failure msg ->
          [Xtmpl.E (("", "div"), [("", "class"), "alert alert-error"], [Xtmpl.D msg])]
    end
;;

let ctype ?(t="text/html; charset=\"utf-8\"") () = ("Content-Type", t);;

let page ?(env=Xtmpl.env_empty) ctx ~title ?javascript ?wtitle ?navpath ?error contents =
  let xmls = Rest_xpage.page ctx.ctx_cfg ~env
    ~title ?javascript ?wtitle ?navpath ?error contents
  in
  Printf.sprintf "<!DOCTYPE html>\n%s\n" (Xtmpl.string_of_xmls xmls)
;;

let page_active v ?(env=Xtmpl.env_empty) =
  let env = Xtmpl.env_add_att ("navbar-"^v) "active" env in
  page ~env
;;

let home_page = page_active "home";;
let tool_page = page_active "tools";;
let filetype_page = page_active "filetypes";;
let chain_page = page_active "chains";;
let in_page = page_active "in";;
let out_page = page_active "out";;
let diff_page = page_active "diff";;

let handle_page_error ?(page=home_page) ctx f x =
  try f x
  with exc ->
      let p msg = Xtmpl.E (("", "p"), [], [Xtmpl.D msg]) in
      let pre msg = Xtmpl.E (("", "pre"), [], [Xtmpl.D msg]) in
      let msg =
        match exc with
        | Sys_error s | Failure s -> p s
        | Loc.Problem pb -> pre (Loc.string_of_problem pb)
        | Ind_io.Error (file, msg) ->
            pre (Printf.sprintf "File %s:\n%s" file msg)
        | Unix.Unix_error (e,s1,s2) ->
            p (Printf.sprintf "%s: %s %s" (Unix.error_message e) s1 s2)
        | e -> p (Printexc.to_string e)
      in
      let title = "Error" in
      let error = Xtmpl.string_of_xml msg in
      ([ctype ()], page ctx ~title ~error [])
;;
let handle_chain_error ctx = handle_page_error ~page: chain_page ctx;;
let handle_outfile_error = handle_page_error ~page: out_page;;
let handle_in_error = handle_page_error ~page: in_page;;

let table ?heads rows =
  let heads =
    match heads with
      None -> []
    | Some heads ->
        let l =
          List.map
          (fun h -> Xtmpl.E (("","th"), [], [Xtmpl.D h]))
          heads
        in
        [
          Xtmpl.E (("","thead"), [],
           [ Xtmpl.E (("","tr"), [], l) ])
        ]
  in
  let td c = Xtmpl.E (("","td"), [], c) in
  let tr l = Xtmpl.E (("","tr"), [], List.map td l) in
  let rows = List.map tr rows in
  Xtmpl.E
  (("", "table"), [("","class"), "table table-bordered table-striped"],
   (heads @ rows)
  )
;;

let ul l =
  let l = List.map (fun c -> Xtmpl.E (("","li"), [], c)) l in
  Xtmpl.E (("","ul"), [], l)
;;

let a ~href contents =
  (Xtmpl.E (("", "a"), [("", "href"), (Rdf_uri.string href)], contents))
;;

let a_by_class ctx iri =
  let cl =
    match Grdfs.class_of ctx.ctx_rdf (Iri iri) with
      None -> ""
    | Some cl -> Grdfs.string_of_class cl
  in
  let name = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api iri in
  [
    Xtmpl.D (match cl with  "" -> "" | _ ->  cl^" ") ;
    a ~href: (Rdf_iri.to_uri iri) [Xtmpl.D name]
  ]
;;

let a_filetypes ctx =
  a ~href: (Rdf_iri.to_uri (Grdfs.iri_filetypes ctx.ctx_cfg.Config.rest_api))
    [ Xtmpl.D Grdfs.suffix_filetypes]
;;

let a_filetype ctx iri =
  let name = Grdf_ftype.name ctx.ctx_rdf iri in
  a ~href: (Rdf_iri.to_uri iri) [ Xtmpl.D name ]
;;

let a_tools ctx =
  a ~href: (Rdf_iri.to_uri (Grdfs.iri_tools ctx.ctx_cfg.Config.rest_api))
    [Xtmpl.D Grdfs.suffix_tools]
;;

let a_tool ctx iri =
  let name = Grdf_tool.name ctx.ctx_rdf iri in
  a ~href: (Rdf_iri.to_uri iri) [Xtmpl.D name]
;;

let a_version ctx iri =
  let name = Grdf_version.name ctx.ctx_rdf iri in
  a ~href: (Rdf_iri.to_uri iri) [Xtmpl.D name]
;;

let a_branch ctx iri =
  let name = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api iri in
  a ~href: (Rdf_iri.to_uri iri) [Xtmpl.D name]
;;

let a_chain ctx ?(full=false) fullname =
  let href = Rdf_iri.to_uri (Chn_types.iri_chain ctx.ctx_cfg.Config.rest_api fullname) in
  let name =
    if full then
      Chn_types.string_of_chain_name fullname
    else
      Chn_types.string_of_chain_basename (Chn_types.chain_basename fullname)
  in
  a ~href [Xtmpl.D name]
;;

let a_chain_module ctx modname =
  let prefix = ctx.ctx_cfg.Config.rest_api in
  let href = Rdf_iri.to_uri (Chn_types.iri_chain_module prefix modname) in
  a ~href [Xtmpl.D (Chn_types.string_of_chain_modname modname)]
;;

let a_fchain_module ctx modname =
  let prefix = ctx.ctx_cfg.Config.rest_api in
  let href = Rdf_iri.to_uri (Chn_types.iri_fchain_module prefix modname) in
  a ~href [Xtmpl.D (Chn_types.string_of_chain_modname modname)]
;;

let a_fchain ctx ?label fullname =
  let prefix = ctx.ctx_cfg.Config.rest_api in
  let href = Rdf_iri.to_uri (Chn_types.iri_fchain prefix fullname) in
  let label = match label with
      None -> Chn_types.string_of_chain_basename (Chn_types.fchain_basename fullname)
    | Some s -> s
  in
  a ~href [Xtmpl.D label]
;;

let a_ichain ctx name =
  let prefix = ctx.ctx_cfg.Config.rest_api in
  let href = Rdf_iri.to_uri (Chn_types.iri_ichain prefix name) in
  a ~href [Xtmpl.D (Chn_types.string_of_ichain_name name)]
;;

let a_outfile ctx path =
  match List.rev path with
    [] -> Xtmpl.D ""
  | basename :: _ ->
      let prefix = ctx.ctx_cfg.Config.rest_api in
      let href = Rdf_iri.to_uri (Grdfs.iri_outfile_path prefix path) in
      a ~href [Xtmpl.D basename]
;;

let a_input ctx ?(full=true) path =
  match List.rev path with
    [] -> Xtmpl.D ""
  | h :: q ->
      let prefix = ctx.ctx_cfg.Config.rest_api in
      let href= Rdf_iri.to_uri (Grdfs.iri_input_path prefix path) in
      let label =
        if full then
          let h = List.hd path and q = List.tl path in
          List.fold_left (fun acc s -> acc ^ "/" ^ s) h q
        else
          h
      in
      a ~href [Xtmpl.D label]
;;

let a_input_file ctx path file_path =
  match List.rev path with
    [] -> Xtmpl.D ""
  | _ ->
      let prefix = ctx.ctx_cfg.Config.rest_api in
      let href= Rdf_iri.to_uri (Grdfs.iri_input_file_path prefix path file_path) in
      let label = match file_path with [] -> assert false | s :: _ -> s in
      a ~href [Xtmpl.D label]
;;

let xhtml_of_ports ctx dir iri =
  let ports = Grdf_port.ports ctx.ctx_rdf iri dir in
  let sep = match dir with Grdf_port.In -> " -&gt; " | Grdf_port.Out -> " * " in
  let f s =
    let iri_ftype = Grdfs.iri_filetype ctx.ctx_cfg.Config.rest_api s in
    Printf.sprintf "<a href=%S>%s</a>" (Rdf_uri.string (Rdf_iri.to_uri iri_ftype)) s
  in
  [Xtmpl.xml_of_string
    (Grdf_port.string_type_of_ports ctx.ctx_rdf
     (Grdf_port.string_of_port_type f)
     ~sep
     ports
    )
  ]
;;

let xhtml_of_intf_type ctx iri =
  (xhtml_of_ports ctx Grdf_port.In iri) @
  [ Xtmpl.D  " -> " ] @
  (xhtml_of_ports ctx Grdf_port.Out iri)
;;

let xhtml_concat sep l =
  let rec iter acc = function
    [] -> List.rev acc
  | h :: q ->
    let acc = h :: sep :: acc in
    iter acc q
  in
  iter [] l
;;

let xhtml_navpath_join_path path =
  (xhtml_concat (Xtmpl.D " / ") (path)) @ [Xtmpl.D " /"]
;;

let navpath_of_tool ctx = [ a_tools ctx ];;

let xhtml_navpath_of_tool ctx =
  xhtml_navpath_join_path (navpath_of_tool ctx)
;;

let xhtml_navpath_of_filetype ctx iri =
  xhtml_navpath_join_path [ a_filetypes ctx ]
;;

let navpath_of_branch ctx ?(inc_iri=false) iri =
  let link iri = a ~href: (Rdf_iri.to_uri iri)
    [Xtmpl.D (Grdfs.name ctx.ctx_rdf (Iri iri))]
  in
  let rec iter acc = function
    None -> acc
  | Some (iri, is_tool) ->
      let acc =
        if is_tool then
          (
           let href = Rdf_iri.to_uri (Grdfs.iri_branches iri) in
           (link iri :: (a ~href [Xtmpl.D Grdfs.suffix_branches]) :: acc)
          )
        else
          link iri :: acc
      in
      iter acc (Grdf_branch.parent ctx.ctx_rdf iri)
  in
  let acc = if inc_iri then [link iri] else [] in
  let p = iter acc (Grdf_branch.parent ctx.ctx_rdf iri) in
  (navpath_of_tool ctx) @ p
;;

let xhtml_navpath_of_branch ctx ?inc_iri iri =
  xhtml_navpath_join_path (navpath_of_branch ctx ?inc_iri iri)
;;

let xhtml_navpath_of_branches ctx iri =
  xhtml_navpath_of_branch ctx ~inc_iri: true iri
;;

let xhtml_navpath_of_version ctx ?inc_iri iri =
  (match Grdf_version.parent ctx.ctx_rdf iri with
     None -> []
   | Some uri -> xhtml_navpath_of_branch ctx ~inc_iri: true iri
  )
;;

let xhtml_navpath_of_versions ctx iri =
  let p = navpath_of_branch ctx ~inc_iri: true iri in
  xhtml_navpath_join_path p
;;

let xhtml_navpath_of_intf ctx iri =
  let tool = Grdf_intf.tool_of_intf iri in
  let iri_intfs = Rdf_iri.to_uri (Grdfs.iri_intfs ~tool) in
  xhtml_navpath_join_path
  [ a_tools ctx ;
    (a_tool ctx tool) ;
    a ~href: iri_intfs [Xtmpl.D Grdfs.suffix_intfs] ;
  ]
;;

let xhtml_navpath_of_intfs ctx iri =
  if Grdfs.is_a_version ctx.ctx_rdf iri then
    xhtml_navpath_of_version ctx ~inc_iri: true iri
  else
    xhtml_navpath_of_branch ctx ~inc_iri: true iri
;;

let navpath_of_chain_module ctx =
  [
    a ~href: (Rdf_iri.to_uri (Grdfs.iri_chains ctx.ctx_cfg.Config.rest_api))
      [Xtmpl.D Grdfs.suffix_chains] ;
  ]

let xhtml_navpath_of_chain_module ctx =
  let path = navpath_of_chain_module ctx in
  xhtml_navpath_join_path path
;;

let xhtml_navpath_of_chain ctx fullname =
  let modname = Chn_types.chain_modname fullname in
  let path =
    (navpath_of_chain_module ctx) @
    [
      a_chain_module ctx modname;
    ]
  in
  xhtml_navpath_join_path path
;;

let navpath_of_fchain_module ctx =
  [
    a ~href: (Rdf_iri.to_uri (Grdfs.iri_fchains ctx.ctx_cfg.Config.rest_api))
      [Xtmpl.D Grdfs.suffix_fchains] ;
  ]

let xhtml_navpath_of_fchain_module ctx =
  let path = navpath_of_fchain_module ctx in
  xhtml_navpath_join_path path
;;

let xhtml_navpath_of_fchain_list ctx fchain_name =
  let modname = Chn_types.fchain_modname fchain_name in
  let path =
    (navpath_of_fchain_module ctx) @
    [
      a_fchain_module ctx modname;
    ]
  in
  xhtml_navpath_join_path path
;;

let navpath_of_fchain ctx fchain_name =
  let modname = Chn_types.fchain_modname fchain_name in
  let name = Chn_types.mk_fchain_name
    (Chn_types.fchain_chainname fchain_name) ""
  in
  (navpath_of_fchain_module ctx) @
  [
    a_fchain_module ctx modname;
    a_fchain ctx name ;
  ]
;;

let xhtml_navpath_of_fchain ctx fchain_name =
  xhtml_navpath_join_path (navpath_of_fchain ctx fchain_name)
;;

let navpath_of_ichain ctx iri =
  match Chn_inst.instance_source ctx iri with
    None -> []
  | Some iri_fchain ->
      match Chn_types.is_iri_fchain ctx iri_fchain with
        None -> []
      | Some fchain_name -> navpath_of_fchain ctx fchain_name
;;

let xhtml_navpath_of_ichain ctx iri =
  xhtml_navpath_join_path (navpath_of_ichain ctx iri)
;;

let xhtml_navpath_of_ichain_op ctx iri_ichain =
  match Chn_types.is_iri_ichain ctx.ctx_cfg.Config.rest_api iri_ichain with
    None -> []
  | Some name ->
      let p = navpath_of_ichain ctx iri_ichain in
      let id = Chn_types.ichain_id name in
      let path = p @ [a ~href: (Rdf_iri.to_uri iri_ichain) [Xtmpl.D id]] in
      xhtml_navpath_join_path path
;;

let xhtml_navpath_of_outfile ctx path =
  match List.rev path with
    [] -> []
  | _ :: q ->
      let f (acc, acc_path) name =
        (a_outfile ctx (List.rev (name :: acc_path)) :: acc,
         name :: acc_path
        )
      in
      let (path, _) = List.fold_left f ([], []) (List.rev q) in
      let path = List.rev path in
      let out_link =
        let href = Grdfs.iri_outfile_path ctx.ctx_cfg.Config.rest_api [] in
        a ~href: (Rdf_iri.to_uri href) [Xtmpl.D Grdfs.suffix_out]
      in
      let path = out_link :: path in
      xhtml_navpath_join_path path
;;

let navpath_of_input ctx path =
  match List.rev path with
    [] -> []
  | _ :: q ->
      let f (acc, acc_path) name =
         prerr_endline (Printf.sprintf "name=%s" name);
        (a_input ctx ~full: false (List.rev (name :: acc_path)) :: acc,
         name :: acc_path
        )
      in
      let (path, _) = List.fold_left f ([], []) (List.rev q) in
      let path = List.rev path in
      let in_link =
        let href = Rdf_iri.to_uri (Grdfs.iri_input_path ctx.ctx_cfg.Config.rest_api []) in
        a ~href [Xtmpl.D Grdfs.suffix_in]
      in
      in_link :: path
;;

let xhtml_navpath_of_input ctx path =
  xhtml_navpath_join_path (navpath_of_input ctx path)
;;

let xhtml_navpath_of_input_file ctx ~input path =
  let path = navpath_of_input ctx (input @ path) in
  xhtml_navpath_join_path path
;;

let xhtml_navpath ctx = function
| `Chains
| `Flat_chains
| `Tools -> []
| `Tool iri -> xhtml_navpath_of_tool ctx
| `Branch iri -> xhtml_navpath_of_branch ctx iri
| `Version iri -> xhtml_navpath_of_version ctx iri
| `Intf iri -> xhtml_navpath_of_intf ctx iri
| `Intfs iri -> xhtml_navpath_of_intfs ctx iri
| `Filetype iri -> xhtml_navpath_of_filetype ctx iri
| `Filetypes -> []
| `Versions iri -> xhtml_navpath_of_versions ctx iri
| `Branches iri -> xhtml_navpath_of_branches ctx iri
| `Chain_module modname -> xhtml_navpath_of_chain_module ctx
| `Chain fullname -> xhtml_navpath_of_chain ctx fullname
| `Flat_chain_module modname -> xhtml_navpath_of_fchain_module ctx
| `Flat_chain name -> xhtml_navpath_of_fchain ctx name
| `Flat_chain_list fchain_name -> xhtml_navpath_of_fchain_list ctx fchain_name
| `Inst_chain iri -> xhtml_navpath_of_ichain ctx iri
| `Out_file path -> xhtml_navpath_of_outfile ctx path
| `Input path -> xhtml_navpath_of_input ctx path
| `Input_file (input, path) -> xhtml_navpath_of_input_file ctx ~input path
| `Inst_chain_op ichain -> xhtml_navpath_of_ichain_op ctx ichain
;;

let intf_list ctx intfs =
  let heads = [ "Name" ; "Type" ] in
  let f intf =
    [ [ a ~href: (Rdf_iri.to_uri intf) [Xtmpl.D (Grdf_intf.name ctx.ctx_rdf intf)] ] ;
      [ Xtmpl.E (("","code"), [], xhtml_of_intf_type ctx intf) ];
    ]
  in
  let rows = List.map f intfs in
  [ table ~heads rows ]
;;

let xhtml_of_intfs_of ctx iri =
  prerr_endline "xhtml_of_intfs: start";
  let (explicit, explicit_no, inherited) = Grdf_intf.compute_intfs_of ctx.ctx_rdf iri in
  prerr_endline "xhtml_of_intfs: explicit,explicit_no,inherited ok";
  let intfs = intf_list ctx (Iriset.elements explicit) in
  let intfs_no = intf_list ctx (Iriset.elements explicit_no) in
  prerr_endline "xhtml_of_intfs: intfs ok";
  let inherited_intfs = intf_list ctx (Iriset.elements (Iriset.diff inherited explicit_no)) in
  prerr_endline "xhtml_of_intfs: inherited ok";
  let tmpl = Rest_xpage.tmpl_file ctx.ctx_cfg "intfs.tmpl" in
  let env = Xtmpl.env_of_list
    [
      ("", "has_intfs"), (fun _ _ _ -> [if Iriset.is_empty explicit then Xtmpl.D "" else Xtmpl.D "true"])  ;
      ("", "intfs"), (fun _ _ _ -> intfs) ;
      ("", "has_intfs_no"), (fun _ _ _ -> [if Iriset.is_empty explicit_no then Xtmpl.D "" else Xtmpl.D "true"])  ;
      ("", "intfs_no"), (fun _ _ _ -> intfs_no) ;
      ("", "has_inherited_intfs"), (fun _ _ _ -> [if Iriset.is_empty inherited then Xtmpl.D "" else Xtmpl.D "true"]) ;
      ("", "inherited_intfs"), (fun _ _ _ -> inherited_intfs) ;
    ]
  in
  let env = Xtmpl.env_of_list ~env (Rest_xpage.default_commands ctx.ctx_cfg) in
  Xtmpl.apply_to_file env (Fname.abs_string tmpl)
;;

let xhtml_of_branches_of ctx iri =
  let branches = Grdf_branch.subs ctx.ctx_rdf iri in
  match branches with
    [] -> [Xtmpl.D "No branch."]
  | _ ->
      let heads = ["Branch"] in
      let f br = [ [ a_branch ctx br ] ] in
      let branches = List.sort Rdf_iri.compare branches in
      let rows = List.map f branches in
      [ table ~heads rows ]
;;

let xhtml_of_versions_of ctx iri =
  let versions = Grdf_version.versions_of ctx.ctx_rdf ~recur: true iri in
  let versions = List.sort Rdf_iri.compare versions in
  let heads = ["Version" ; "Active" ; "Date"] in
  let f version =
    let active = if Grdfs.is_active_iri ctx.ctx_rdf version then "yes" else "no" in
    let date =
      match Grdfs.creation_date_iri ctx.ctx_rdf version with
        None -> ""
      | Some d -> Netdate.mk_mail_date (Netdate.since_epoch d)
    in
    [ [a_version ctx version] ; [Xtmpl.D active] ; [Xtmpl.D date] ]
  in
  let rows = List.map f versions in
  [ table ~heads rows ]
;;

let get_tool ctx iri =
  let name = Grdf_tool.name ctx.ctx_rdf iri in
  let dot = Grdf_dot.dot_of_tool ctx.ctx_rdf iri in
  let svg = dot_to_svg dot in
  let branches = xhtml_of_branches_of ctx iri in
  let intfs = xhtml_of_intfs_of ctx iri in
  let tmpl = Rest_xpage.tmpl_file ctx.ctx_cfg "tool.tmpl" in
  let env = Xtmpl.env_of_list
    [
      ("", "graph"), (fun _ _ _ -> svg) ;
      ("", "branches"), (fun _ _ _ -> branches) ;
      ("", "interfaces"), (fun _ _ _ -> intfs) ;
    ]
  in
  let env = Xtmpl.env_of_list ~env (Rest_xpage.default_commands ctx.ctx_cfg) in
  let contents = Xtmpl.apply_to_file env (Fname.abs_string tmpl) in
  let navpath = xhtml_navpath ctx (`Tool iri) in
  ([ctype ()], tool_page ctx ~title: name ~navpath contents)
;;

let get_tools ctx =
  let wld = ctx.ctx_rdf in
  let tools = Grdf_tool.tools wld in
  let f_tool iri =
    let n_versions = List.length (Grdf_version.versions_of wld ~recur: true iri) in
    let href_versions = Grdfs.iri_versions iri in

    let n_branches = List.length (Grdf_branch.subs wld ~recur: false iri) in
    let href_branches = Grdfs.iri_branches iri in

    let n_intfs = Iriset.cardinal (Grdf_intf.intfs_of_tool wld iri) in
    let href_intfs = Grdfs.iri_intfs iri in

    [ [ a ~href: (Rdf_iri.to_uri iri) [Xtmpl.D (Grdf_tool.name wld iri)] ] ;
      [ a ~href: (Rdf_iri.to_uri href_versions) [Xtmpl.D (string_of_int n_versions)] ] ;
      [ a ~href: (Rdf_iri.to_uri href_branches) [Xtmpl.D (string_of_int n_branches)] ] ;
      [ a ~href: (Rdf_iri.to_uri href_intfs) [Xtmpl.D (string_of_int n_intfs)] ] ;
    ]
  in
  let heads = [ "Name" ; "Versions" ; "Branches" ; "Interfaces" ] in
  let tools = List.sort Rdf_iri.compare tools in
  let rows = List.map f_tool tools in
  let contents = [table ~heads rows] in
  ([ctype ()], tool_page ctx ~title: "Tools" contents)
;;

let get_intf ctx iri =
  let name = Grdf_intf.name ctx.ctx_rdf iri in
  let tool = Grdf_intf.tool_of_intf iri in
  let tool_name = Grdf_tool.name ctx.ctx_rdf tool in
  let wtitle = Printf.sprintf "%s / %s" tool_name name in
  let typ =
    let of_dir label dir =
      Xtmpl.E (("", "p"), [],
       [ Xtmpl.E (("","strong"), [], [Xtmpl.D label]) ;
         Xtmpl.E (("","code"), [],  (xhtml_of_ports ctx dir iri) );
       ])
    in
    [
      of_dir "Input : " Grdf_port.In ;
      of_dir "Output : " Grdf_port.Out ;
    ]
  in
  let branches_yes =
    match Grdf_intf.implementors ctx.ctx_rdf iri with
      [] -> prerr_endline "empty branches_yes"; []
    | l -> [ ul (List.map (a_by_class ctx) l) ]
  in
  let branches_no =
    match Grdf_intf.not_implementors ctx.ctx_rdf iri with
      [] -> prerr_endline "empty branches_no"; []
    | l -> [ ul (List.map (a_by_class ctx) l) ]
  in
  let tool = a ~href: (Rdf_iri.to_uri tool) [Xtmpl.D tool_name] in
  let path =
    Misc.string_of_opt (Grdf_intf.command_path ctx.ctx_rdf iri)
  in
  let tmpl = Rest_xpage.tmpl_file ctx.ctx_cfg "intf.tmpl" in
  let env = Xtmpl.env_of_list
    [
      ("", "type"), (fun _ _ _ -> typ) ;
      ("", "path"), (fun _ _ _ -> [Xtmpl.D path]) ;
      ("", "tool"), (fun _ _ _ -> [tool]) ;
      ("", "branches_yes"), (fun _ _ _ -> branches_yes) ;
      ("", "branches_no"), (fun _ _ _ -> branches_no) ;
    ]
  in
  let env = Xtmpl.env_of_list ~env (Rest_xpage.default_commands ctx.ctx_cfg) in
  let navpath = xhtml_navpath ctx (`Intf iri) in
  let contents = Xtmpl.apply_to_file env (Fname.abs_string tmpl) in
  ([ctype ()], tool_page ctx ~title: name ~wtitle ~navpath contents)
;;

let get_intfs ctx iri =
  let pre = "Interfaces of " in
  let name = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api iri in
  let wtitle = Printf.sprintf "%s %s" pre name in
  let title = Printf.sprintf "%s %s" pre
    (Xtmpl.string_of_xml (a ~href: (Rdf_iri.to_uri iri) [Xtmpl.D name]))
  in
  let contents = xhtml_of_intfs_of ctx iri in
  let navpath = xhtml_navpath ctx (`Intfs iri) in
  ([ctype ()], tool_page ctx ~title ~wtitle ~navpath contents)
;;

let get_filetype ctx iri =
  let name = Grdf_ftype.name ctx.ctx_rdf iri in
  let contents = [Xtmpl.D name] in
  let navpath = xhtml_navpath ctx (`Filetype iri) in
  ([ctype ()], filetype_page ctx ~title: name ~navpath contents)
;;

let get_filetypes ctx =
  let ftypes = Grdf_ftype.filetypes ctx.ctx_rdf in
  let heads = [ "Name" ; "Extension" ; "Description" ] in
  let wld = ctx.ctx_rdf in
  let f iri =
    [ [ a_filetype ctx iri ];
      [ Xtmpl.D (Grdf_ftype.extension wld iri) ] ;
      [ Xtmpl.D (Grdf_ftype.desc wld iri) ];
    ]
  in
  let rows = List.map f ftypes in
  let contents = [ table ~heads rows ] in
  ([ctype ()], filetype_page ctx ~title: "Filetypes" contents)
;;

let get_version ctx iri =
  let wtitle = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api iri in
  let name = Grdf_version.name ctx.ctx_rdf iri in
  let title = Xtmpl.string_of_xml (a ~href: (Rdf_iri.to_uri iri) [Xtmpl.D name]) in
  let contents = xhtml_of_intfs_of ctx iri in
  let navpath = xhtml_navpath ctx (`Version iri) in
  ([ctype ()], tool_page ctx ~title ~wtitle ~navpath contents)
;;

let get_versions ctx tool =
  let contents = xhtml_of_versions_of ctx tool in
  let title = Printf.sprintf "Versions of %s" (Grdf_tool.name ctx.ctx_rdf tool) in
  let navpath = xhtml_navpath ctx (`Versions tool) in
  ([ctype ()], tool_page ctx ~title ~navpath contents)
;;

let get_branch ctx iri =
  prerr_endline "get_branch: start";
  let name = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api iri in
  prerr_endline "get_branch: name ok";
  let dot = Grdf_dot.dot_of_branch ctx.ctx_rdf iri in
  let svg = dot_to_svg dot in
  prerr_endline "get_branch: svg ok";
  let branches = xhtml_of_branches_of ctx iri in
  prerr_endline "get_branch: branches ok";
  let intfs = xhtml_of_intfs_of ctx iri in
  prerr_endline "get_branch: intfs ok";
  let versions = xhtml_of_versions_of ctx iri in
  prerr_endline "get_branch: versions ok";
  let tmpl = Rest_xpage.tmpl_file ctx.ctx_cfg "branch.tmpl" in
  let env = Xtmpl.env_of_list
    [
      ("", "graph"), (fun _ _ _ -> svg) ;
      ("", "branches"), (fun  _ _ _ -> branches) ;
      ("", "interfaces"), (fun _ _ _ -> intfs) ;
      ("", "versions"), (fun _ _ _ -> versions) ;
    ]
  in
  let env = Xtmpl.env_of_list ~env (Rest_xpage.default_commands ctx.ctx_cfg) in
  let contents = Xtmpl.apply_to_file env (Fname.abs_string tmpl) in
  let navpath = xhtml_navpath ctx (`Branch iri) in
  ([ctype ()], tool_page ctx ~title: name ~navpath contents)
;;

let get_branches ctx iri =
  let pre = "Branches of " in
  let name = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api iri in
  let wtitle = Printf.sprintf "%s %s" pre name in
  let title = Printf.sprintf "%s %s" pre
    (Xtmpl.string_of_xml (a ~href: (Rdf_iri.to_uri iri) [Xtmpl.D name]))
  in
  let contents = xhtml_of_branches_of ctx iri in
  let navpath = xhtml_navpath ctx (`Branches iri) in
  ([ctype ()], tool_page ctx ~title ~wtitle ~navpath contents)
;;

let get_root ctx =
  let dot = Grdf_dot.dot ~edge_labels: false ctx.ctx_rdf in
  let svg = dot_to_svg dot in
  let title = ctx.ctx_cfg.Config.project_name in
  let contents = svg in
  ([ctype ()], home_page ctx ~title contents)

let get_chains ctx =
  let chain_files = Chn_io.chain_files ctx.ctx_cfg in
  let modules = List.map Chn_io.modname_of_file chain_files in
  let modules = List.sort Chn_types.compare_chain_modname modules in
  let rows = List.map (fun m -> [[a_chain_module ctx m]]) modules in
  let heads = ["Chain module"] in
  let (deps, error) =
    let config = ctx.ctx_cfg in
    let (cmods, errors) = Chn_io.load_chain_files config in
    let deps = Chn_ast.compute_deps ctx.ctx_rdf config cmods in
    let dot = Rest_xpage.dot_of_deps config.Config.rest_api deps in
    let svg = dot_to_svg dot in
    let error =
      match errors with
        [] -> ""
      | _ ->
          Printf.sprintf "<pre><![CDATA[%s]]></pre>"
          (String.concat "\n" errors)
    in
    (svg, error)
  in
  let contents =
    [
      Xtmpl.E (("", "section"), [], [table ~heads rows]) ;
      Xtmpl.E (("", "section"), [("","title"), "Dependencies"], deps) ;
    ]
  in
  let title = "Modules" in
  ([ctype ()], chain_page ctx ~title ~error contents)
;;

let get_chain_module ctx ?nav modname =
  let config = ctx.ctx_cfg in
  let file = Chn_io.file_of_modname config modname in
  let title = Printf.sprintf "Module %s"
    (Chn_types.string_of_chain_modname modname)
  in
  let cmod = Chn_io.chn_module_of_file file in
  let heads = ["Chain" ; "Number of flat chains"] in
  let rows = List.map
    (fun chain ->
       let name = Chn_types.mk_chain_name modname
         chain.Chn_ast.chn_name
       in
       let flats = Chn_flat.flat_chains_of_chain ctx name in
       let href = 
         Rdf_iri.to_uri 
           (Chn_types.iri_fchain config.Config.rest_api
            (Chn_types.mk_fchain_name name ""))
       in
       let text = string_of_int (List.length flats) in
       [[a_chain ctx name] ; [a ~href [Xtmpl.D text]]]
    )
    (List.sort Pervasives.compare cmod.Chn_ast.cmod_chains)
  in
  let deps =
    let deps = Chn_ast.compute_deps ctx.ctx_rdf config [cmod] in
    let dot =
      Rest_xpage.dot_of_deps config.Config.rest_api
      ~fullnames: false deps
    in
    dot_to_svg dot
  in
  let contents =
    [
      Xtmpl.E (("", "section"), [], [table ~heads rows]) ;
      Xtmpl.E (("", "section"), [("","title"), "Dependencies"], deps) ;
    ]
  in
  let navpath =
    let spec =
      match nav with
        None -> `Chain_module modname
      | Some x -> x
    in
    xhtml_navpath ctx spec
  in
  ([ctype ()], chain_page ctx ~title ~navpath contents)
;;

let get_chain ctx fullname =
  let title = Chn_types.string_of_chain_name fullname in
  let config = ctx.ctx_cfg in
  let file = Chn_io.file_of_modname config (Chn_types.chain_modname fullname) in
  let cmod = Chn_io.chn_module_of_file file in
  let basename = Chn_types.chain_basename fullname in
  let (code, svg) =
    match Chn_ast.get_chain cmod basename with
      None -> ([Xtmpl.D "No code found"], [Xtmpl.D ""])
    | Some chn ->
        let prefix = config.Config.rest_api in
        let code = Rest_xpage.xhtml_of_chain prefix chn in
        let svg =
          let dot = Rest_xpage.dot_of_chain prefix chn in
          dot_to_svg dot
        in
        (code, svg)
  in
  let navpath = xhtml_navpath ctx (`Chain fullname) in
  let contents =
    [
      Xtmpl.E (("", "section"), [], svg) ;
      Xtmpl.E (("", "section"),
       [("","title"), "Source from "^(Fname.quote file)], code) ;
    ]
  in
  ([ctype ()], chain_page ctx ~title ~navpath contents)
;;


let get_fchains ctx = get_chains ctx;;

let get_fchain_module ctx modname =
  get_chain_module ctx ~nav: (`Flat_chain_module modname) modname
;;

let get_fchain_list ctx fchain_name =
  let chain_name = Chn_types.fchain_chainname fchain_name in
  let s_chain_name = Chn_types.string_of_chain_name chain_name in
  let wtitle = Printf.sprintf "Flat chains from %s" s_chain_name in
  let title = Printf.sprintf "Flat chains from %s" (Xtmpl.string_of_xml (a_chain ctx chain_name)) in
  let navpath = xhtml_navpath ctx (`Flat_chain_list fchain_name) in
  let flats = Chn_flat.flat_chains_of_chain ctx chain_name in
  let heads = [ "Commit id" ; "Flatten date" ] in
  let rows = List.map
    (fun iri ->
       let id =
         match Chn_types.is_iri_fchain ctx iri with
           None -> []
         | Some name ->
             match Chn_types.fchain_id name with
               None -> []
             | Some id -> [a ~href: (Rdf_iri.to_uri iri) [Xtmpl.D id]]
       in
       let date = Misc.map_opt Netdate.since_epoch (Grdfs.creation_date_iri ctx.ctx_rdf iri) in
       (id, date)
    )
    flats
  in
  let comp (_,d1) (_,d2) =
    match d1, d2 with
      None, None -> 0
    | Some _, None -> -1
    | None, Some _ -> 1
    | Some d1, Some d2 -> Pervasives.compare d2 d1
  in
  let rows = List.sort comp rows in
  let rows = List.map
    (fun (id, d) ->
       let d = match d with None -> [] | Some d -> [Xtmpl.D (Netdate.mk_mail_date d)] in
       [ id ; d]
    )
    rows
  in
  let contents = [ table ~heads rows ] in
  ([ctype ()], chain_page ctx ~title ~wtitle ~navpath contents)
;;

let xhtml_inst_list ctx list =
  let heads = [ "" ; "" ; "Execution" ; "Date" ] in
  let list = List.map
    (fun iri -> (iri, Grdfs.creation_date_iri ctx.ctx_rdf iri)) list
  in
  let list = List.sort
    (fun (_,d1) (_,d2) -> Pervasives.compare d2 d1) list
  in
  let rows = List.map
    (fun (iri_inst, date) ->
       match Chn_types.is_iri_ichain ctx.ctx_cfg.Config.rest_api iri_inst with
         None -> [ [Xtmpl.D ("bad ichain "^(Rdf_iri.string iri_inst))] ; [Xtmpl.D ""] ]
       | Some name ->
           let date =
             match date with
               None -> ""
             | Some d -> Netdate.mk_mail_date (Netdate.since_epoch d)
           in
           let a_ichain =
             let link = a_ichain ctx name in
             match Chn_inst.inst_is_reference ctx iri_inst with
               false ->  [link]
             | true -> [link ; Xtmpl.D " " ; Rest_xpage.star ~label: "Reference instanciation chain" ()]
           in
           let mk_option n =
             [ Xtmpl.E (("","input"),
                [ ("","type"),"radio" ;
                  ("","name"), "inst"^(string_of_int n) ;
                  ("","value"), Rdf_iri.string iri_inst],
                [])
             ]
           in
           let inst1_option = mk_option 1 in
           let inst2_option = mk_option 2 in
           [  inst1_option ; inst2_option ; a_ichain ; [Xtmpl.D date] ]
    )
    list
  in
  let table = table ~heads rows in
  let action =
    let iri = Rdf_iri.concat ctx.ctx_cfg.Config.rest_api Grdfs.suffix_diff in
    let iri = Rdf_iri.concat iri Grdfs.suffix_ichains in
    Rdf_iri.string iri
  in
  Xtmpl.E (("", "form"), [("", "action"), action],
    [ table ;
      Xtmpl.E (("","input"), [("","value"),"Show diffs" ; ("","type"), "submit"], []) ;
    ])
;;

let get_fchain ctx iri =
  let fchain_name =
    match Chn_types.is_iri_fchain ctx iri with
      None -> failwith (Printf.sprintf "Unknown flat chain %S" (Rdf_iri.string iri))
    | Some n -> n
  in
  let module_ids =
    let heads = [ "Module" ; "Commit id" ] in
    let versions = Chn_flat.fchain_chain_versions ctx iri in
    let rows = Chn_flat.Chain_versions.fold
      (fun (mn, id) acc ->
         [ [ Xtmpl.D (Chn_types.string_of_chain_modname mn) ] ;
           [ Xtmpl.D id ]
         ] :: acc) versions []
    in
    let rows = List.sort Pervasives.compare rows in
    [ table ~heads rows ]
  in

  let title = "" in
  let id = Chn_types.fchain_id fchain_name in
  let wtitle = Printf.sprintf "%s (%s)"
    (Chn_types.string_of_chain_name (Chn_types.fchain_chainname fchain_name))
    (Misc.string_of_opt id)
  in
  let navpath = xhtml_navpath ctx (`Flat_chain fchain_name) in
  let svg =
    let dot = Rest_xpage.dot_of_fchain ctx fchain_name in
    dot_to_svg ~svg_h: 600 dot
  in
  let ichains =
    let l = Chn_inst.instances ctx iri in
    match l with
      [] -> []
    | _ ->
       let l = List.map (fun (iri, _, _) -> iri) l in
        [ Xtmpl.E (("","section"), [("","title"), "Instanciated chains"], [xhtml_inst_list ctx l]) ]
  in
(*  let iri_fchain = Chn_types.iri_fchain ctx.ctx_cfg.Config.rest_api fchain_name in*)
  let date = Misc.string_of_opt (Chn_flat.fchain_creation_date ctx iri) in
  let contents =
    [
      Xtmpl.E (("", "p"), [],
       [ Xtmpl.E (("","strong"), [], [ Xtmpl.D "Id:" ]);
         Xtmpl.D (Misc.string_of_opt id) ;
       ]) ;
      Xtmpl.E (("", "p"), [],
       [ Xtmpl.E (("","strong"), [], [ Xtmpl.D "Creation date:" ]);
         Xtmpl.D date ;
       ]) ;
      Xtmpl.E (("", "p"), [],
       (Xtmpl.E (("","strong"), [], [ Xtmpl.D "Module ids:" ])) ::
         module_ids
       )
    ] @ svg @ ichains
  in
  ([ctype ()], chain_page ctx ~title ~wtitle ~navpath contents)
;;

let get_ichain_op ctx iri =
  let inst_chain =
    match Grdfs.subject_iri ctx.ctx_rdf
      ~pred: Grdfs.genet_containsop ~obj: (Iri iri)
    with
      None -> failwith (Printf.sprintf "%S is not contained" (Rdf_iri.string iri))
    | Some u -> u
  in
  let ichain_name =
    match Chn_types.is_iri_ichain ctx.ctx_cfg.Config.rest_api inst_chain with
      None -> assert false
    | Some n -> n
  in
  let op_name = Chn_types.ichain_op_name ~ichain: inst_chain iri in
  let start_date = Misc.string_of_opt (Chn_run.ichain_start_date ctx iri) in
  let stop_date = Misc.string_of_opt (Chn_run.ichain_stop_date ctx iri) in
  let title = op_name in
  let wtitle = Printf.sprintf "%s/%s" (Chn_types.string_of_ichain_name ichain_name) op_name in
  let navpath = xhtml_navpath ctx (`Inst_chain_op inst_chain) in
  let iri_from = Chn_flat.get_op_origin ctx iri in
  let interface =
    match Grdf_intf.intf_exists ctx.Chn_types.ctx_rdf iri_from with
      None -> []
    | Some name ->
        let tool = Grdf_intf.tool_of_intf iri_from in
        let name = Printf.sprintf "%s / %s" (Grdf_tool.name ctx.Chn_types.ctx_rdf tool) name in
        [
          Xtmpl.E (("", "p"), [],
           [ Xtmpl.E (("","strong"), [], [ Xtmpl.D "Interface:" ]);
             a ~href: (Rdf_iri.to_uri iri_from) [Xtmpl.D name] ;
           ]) ;
        ]
  in
  let return_code =
    match Chn_run.return_code ctx iri with
      0 -> []
    | n ->
        [
          Xtmpl.E (("","p"), [],
            [ Xtmpl.E (("","strong"), [], [Xtmpl.D "Return code:"]) ;
              a_outfile ctx [string_of_int n] ;
           ])
        ]
  in
  let output =
    match Grdfs.object_literal ctx.ctx_rdf
      ~sub: (Rdf_term.Iri iri) ~pred: Grdfs.genet_commandoutput
    with
      None -> []
    | Some md5 ->
        [
          Xtmpl.E (("","p"), [],
            [ Xtmpl.E (("","strong"), [], [Xtmpl.D "Output:"]) ;
              a_outfile ctx [md5] ;
           ])
        ]
  in
  let svg =
    let dot = Rest_xpage.dot_of_ichain_op ctx iri in
    dot_to_svg ~svg_h: 600 dot
  in
  let contents =
    [
      Xtmpl.E (("", "p"), [],
       [ Xtmpl.E (("","strong"), [], [ Xtmpl.D "Start date:" ]);
         Xtmpl.D start_date ;
       ]) ;
      Xtmpl.E (("", "p"), [],
       [ Xtmpl.E (("","strong"), [], [ Xtmpl.D "Stop date:" ]);
         Xtmpl.D stop_date ;
       ]) ;
    ] @ interface @ return_code @ output @ svg
  in
  ([ctype ()], chain_page ctx ~title ~wtitle ~navpath contents)
;;

let xhtml_flat_chain_of_ichain ctx iri =
  match Chn_inst.instance_source ctx iri with
    None -> [Xtmpl.D "?"]
  | Some iri ->
      match Chn_types.is_iri_fchain ctx iri with
        None -> [Xtmpl.D "?"]
      | Some flat_name ->
          let chain_name = Chn_types.fchain_chainname flat_name in
          let chain = Chn_types.string_of_chain_name chain_name in
          let label = Misc.string_of_opt (Chn_types.fchain_id flat_name) in
          let fchain = a_fchain ctx ~label flat_name in
          [ Xtmpl.D chain ; Xtmpl.D " [" ; fchain ; Xtmpl.D "]" ]
;;

let xhtml_input_of_ichain ctx iri =
  match Chn_inst.inst_input ctx iri with
    None -> [Xtmpl.D "??"]
  | Some (name, id) ->
      [ Xtmpl.D (Printf.sprintf "%s [%s]" (Fname.rel_string name) id) ]
;;

let xhtml_tools_of_ichain ctx ?(short=false) iri =
  let map = Chn_inst.inst_versions ctx iri in
  let l = Irimap.fold (fun tool v acc -> (tool, v) :: acc) map [] in
  match short with
    false ->
      let heads = ["Tool" ; "Version"] in
      let rows = List.map
        (fun (tool, version) ->
           [ [ a_tool ctx tool ] ; [ a_version ctx version ] ]
        )
        l
      in
      [ table ~heads rows ]
  | true ->
     let rec iter = function
        [] -> []
      | (tool, version) :: q ->
          [ a_tool ctx tool ; Xtmpl.D " " ; a_version ctx version ] @
          (match q with
             [] -> []
           |  _-> (Xtmpl.D ", ") :: (iter q)
          )
      in
      iter l
;;



let get_ichain ctx iri =
  let ichain_name =
    match Chn_types.is_iri_ichain ctx.ctx_cfg.Config.rest_api iri with
      None -> failwith (Printf.sprintf "Not an instanciated chain iri: %S" (Rdf_iri.string iri))
    | Some n -> n
  in
  let title = "" in
  let wtitle = Chn_types.string_of_ichain_name ichain_name in
  let navpath = xhtml_navpath ctx (`Inst_chain iri) in
  let svg =
    let dot = Rest_xpage.dot_of_ichain ctx ichain_name in
    dot_to_svg ~svg_h: 600 dot
  in
  let date = Misc.string_of_opt (Chn_flat.fchain_creation_date ctx iri) in
  let start_date = Misc.string_of_opt (Chn_run.ichain_start_date ctx iri) in
  let stop_date = Misc.string_of_opt (Chn_run.ichain_stop_date ctx iri) in
  let tool_versions = xhtml_tools_of_ichain ctx iri in
  let input_info = xhtml_input_of_ichain ctx iri in
  let flat_iri = xhtml_flat_chain_of_ichain ctx iri in
  let exec_error =
    match Grdfs.object_iri ctx.ctx_rdf
      ~sub: (Rdf_term.Iri iri) ~pred: Grdfs.genet_failedcommand
    with
      None -> []
    | Some err_iri ->
        let s_iri = Rdf_iri.string err_iri in
        [ Xtmpl.E (("","p"), [("","class"), "alert alert-error"],
           [ Xtmpl.D "Failed while running " ;
             Xtmpl.E (("","tt"), [],
               [ a ~href: (Rdf_iri.to_uri err_iri) [Xtmpl.D s_iri] ])
           ]
          )
        ]
  in
  let contents =
    [ Xtmpl.E (("","p"), [],
       [ Xtmpl.E (("","strong"), [], [Xtmpl.D "Creation date:"]) ;
         Xtmpl.D date ;
       ]) ;
      Xtmpl.E (("","p"), [],
       [ Xtmpl.E (("","strong"), [], [Xtmpl.D "Start date:"]) ;
         Xtmpl.D start_date ;
       ]) ;
      Xtmpl.E (("","p"), [],
       [ Xtmpl.E (("","strong"), [], [Xtmpl.D "Stop date:"]) ;
         Xtmpl.D stop_date ;
       ]) ;
      Xtmpl.E (("","p"), [],
       ( (Xtmpl.E (("","strong"), [], [Xtmpl.D "Input:"])) :: input_info)
      );
      Xtmpl.E (("","p"), [],
       ( (Xtmpl.E (("","strong"), [], [Xtmpl.D "Flat chain:"])) :: flat_iri)
      );
    ]
    @ exec_error
    @ tool_versions
    @svg
  in
  ([ctype ()], chain_page ctx ~title ~wtitle ~navpath contents)
;;

let get_outfile_raw ctx path =
  let filename =
    List.fold_left Fname.concat_s (Config.out_dir ctx.ctx_cfg) path
  in
  let file_contents = Misc.string_of_file (Fname.abs_string filename) in
  let ctype = ctype ~t: (Misc.file_mimetype (Fname.abs_string filename)) () in
  ([ctype], file_contents)
;;

let file_date file =
  try Netdate.mk_mail_date (Unix.stat file).Unix.st_mtime
  with Unix.Unix_error _ -> ""
;;

let xhtml_dir_contents f_iri dir =
  let entries = Find.find_list Find.Ignore [dir] [Find.Maxdepth 1] in
  let entries =
    (* sort in reverse order because of the fold_left below *)
    List.sort (fun f1 f2 -> Pervasives.compare f2 f1) entries
  in
  let rows =
    List.fold_left (fun acc file ->
       if file = dir then
         acc
       else
         (
          let base = Filename.basename file in
          let href = Rdf_iri.to_uri (f_iri base) in
          [ [ a ~href [Xtmpl.D base] ] ] :: acc
         )
    )
    []
    entries
  in
  table rows
;;

let xhtml_outdir_contents ctx path =
  let dir = List.fold_left Fname.concat_s (Config.out_dir ctx.ctx_cfg) path in
  let prefix = ctx.ctx_cfg.Config.rest_api in
  let f_iri basename =
    Grdfs.iri_outfile_path ~raw: false prefix (path@[basename])
  in
  xhtml_dir_contents f_iri (Fname.abs_string dir)
;;

let xhtml_inputdir_contents ctx input path =
  let dir =
    List.fold_left Fname.concat_s (Config.data_dir ctx.ctx_cfg) (input @ path)
  in
  let prefix = ctx.ctx_cfg.Config.rest_api in
  let f_iri basename =
    Grdfs.iri_input_file_path ~raw: false prefix input (path@[basename])
  in
  xhtml_dir_contents f_iri (Fname.abs_string dir)
;;

let get_outfile ctx path raw =
  match raw with
    true -> get_outfile_raw ctx path
  | false ->
      let prefix = ctx.ctx_cfg.Config.rest_api in
      let filename =
        List.fold_left Fname.concat_s (Config.out_dir ctx.ctx_cfg) path
      in
      let wtitle = String.concat "/" path in
      let kind = (Unix.lstat (Fname.abs_string filename)).Unix.st_kind in
      let raw_link =
        match kind with
          Unix.S_DIR -> []
        | _ ->
            let href = Rdf_iri.to_uri (Grdfs.iri_outfile_path ~raw: true prefix path) in
            [ Xtmpl.D " [";
              a ~href [Xtmpl.D "raw"] ;
              Xtmpl.D "]" ;
            ]
      in
      let title =
        Printf.sprintf "%s%s"
        (match List.rev path with [] -> assert false | s :: _ -> s)
        (Xtmpl.string_of_xmls raw_link)
      in
      let file_contents =
        match kind with
          Unix.S_DIR ->
            xhtml_outdir_contents ctx path
        | _ ->
            let file_contents = Misc.string_of_file (Fname.abs_string filename) in
            Xtmpl.E (("", "hcode"), [], [Xtmpl.D file_contents])
      in
      let contents =
        [ Xtmpl.E (("","p"), [],
           [ Xtmpl.E (("","strong"), [], [ Xtmpl.D "Date:" ] ) ;
             Xtmpl.D  (file_date (Fname.abs_string filename)) ;
           ]) ;
          Xtmpl.E (("","p"), [],
           [ Xtmpl.E (("","div"), [("","class"), "btn-group"],
              [ Xtmpl.E (("","a"),
                 [(("","class"), "btn") ;
                   (("","href"), Rdf_iri.string (Grdfs.iri_ichains_producers_of prefix path))
                 ],
                 [ Xtmpl.D "Producers" ])
              ]);
           ]);
           file_contents
        ]
      in
      let navpath = xhtml_navpath ctx (`Out_file path) in
      ([ctype ()], out_page ctx ~title ~wtitle ~navpath contents)
;;

let xhtml_inst_list_of_ports ctx ports =
  let heads = [ "Instanciated chain" ; "Port" ; "Date" ] in
  let f acc port =
    let v =
      let pname =
        match Grdf_port.port_name ctx.ctx_rdf port with
          "" -> string_of_int (Grdf_port.port_rank port)
        | s -> s
      in
      let container = Grdfs.port_container port in
      match Grdfs.is_a_instopn ctx.ctx_rdf container with
        false -> None (* do not make ichain ports appear, but only ichain operation ports *)
      | true ->
          let origin = Chn_flat.get_op_origin ctx container in
          let name =
            match Grdfs.name ctx.ctx_rdf (Rdf_term.Iri origin) with
              "" -> pname
            | s -> Printf.sprintf "%s.%s" s pname
          in
          let inst =
            match Grdfs.subject_iri ctx.ctx_rdf
              ~pred: Grdfs.genet_containsop ~obj: (Rdf_term.Iri container)
            with
              None ->
                failwith
                (Printf.sprintf "No instanciated chain ??? (container=%S)" (Rdf_iri.string  container))
            | Some inst -> inst
          in
          Some (name, inst)
    in
    match v with
      None -> acc
    | Some (port_name, inst) ->
        let a_inst =
          match Chn_types.is_iri_ichain ctx.ctx_cfg.Config.rest_api inst with
            None -> Xtmpl.D (Printf.sprintf "%S is not an inst. chain" (Rdf_iri.string inst))
          | Some icname -> a_ichain ctx icname
        in
        [ [ a_inst ] ;
          [ Xtmpl.D port_name ] ;
          [ Xtmpl.D (Misc.string_of_opt (Chn_flat.fchain_creation_date ctx inst))] ;
        ] :: acc
  in
  let rows = List.fold_left f [] ports in
  [ table ~heads rows ]
;;

let get_inst_producers_of ctx path =
  match path with
    [] -> assert false
  | md5 :: _ ->
      (* get the producers of the file *)
      let ports = Grdfs.subject_iris ctx.ctx_rdf
        ~pred: Grdfs.genet_filemd5
        ~obj: (Rdf_term.term_of_literal_string md5)
      in
      let table = xhtml_inst_list_of_ports ctx ports in
      let title = Printf.sprintf "Producers of %s" (Xtmpl.string_of_xml (a_outfile ctx [md5])) in
      let wtitle = Printf.sprintf "Producers of %s" md5 in
      ([ctype ()], chain_page ctx ~title ~wtitle table)
;;

let get_inputs ctx =
  let inputs = Ind_io.list_inputs ctx.ctx_cfg in
  let heads = ["Input"] in
  let rows = List.map (fun s -> [ [a_input ctx [Fname.rel_string s]]] ) inputs in
  let table = [ table ~heads rows ] in
  let title = "Inputs" in
  ([ctype ()], in_page ctx ~title table)
;;

let get_input ctx path =
  let dirname =
    List.fold_left Fname.concat_s (Config.data_dir ctx.ctx_cfg) path
  in
  let title = String.concat "/" path in
  let spec = Ind_io.load ctx.ctx_cfg dirname in
  let git_id = Misc.get_git_id (Fname.abs_string dirname) in
  let in_table =
    let f (name, _) =
      let file_path = Misc.split_filename (Fname.rel_string name) in
      [ [ a_input_file ctx path file_path ] ]
    in
    let rows = List.map f spec.Ind_types.in_files in
    table rows
  in
  let chains_table =
     let f name =
       let chain_name = Chn_types.chain_name_of_string name in
       [ [a_chain ctx ~full:true chain_name] ]
    in
    let rows = List.map f spec.Ind_types.chains in
    table rows
  in
  let contents =
    [ Xtmpl.E (("","p"), [],
       [ Xtmpl.E (("","strong"), [], [Xtmpl.D "Id:"]) ;
         Xtmpl.D git_id ;
       ]) ;
      Xtmpl.E (("","h2"), [], [Xtmpl.D "Inputs"]);
      in_table ;
      Xtmpl.E (("","h2"), [], [Xtmpl.D "Chains"]);
      chains_table ;
    ]
  in
  let navpath = xhtml_navpath ctx (`Input path) in
  ([ctype ()], in_page ctx ~title ~navpath contents)
;;

let get_input_file ctx ~raw ~input file_path =
  let filename =
    List.fold_left Fname.concat_s (Config.data_dir ctx.ctx_cfg) (input @ file_path)
  in
  match raw with
    true ->
      let file_contents = Misc.string_of_file (Fname.abs_string filename) in
      let ctype = ctype ~t: (Misc.file_mimetype (Fname.abs_string filename)) () in
      ([ctype], file_contents)
  | false ->
      let prefix = ctx.ctx_cfg.Config.rest_api in
      let kind = (Unix.lstat (Fname.abs_string filename)).Unix.st_kind in
      let contents =
        match kind with
          Unix.S_DIR ->
            [ xhtml_inputdir_contents ctx input file_path ]
        | _ ->
            let file_contents = Misc.string_of_file (Fname.abs_string filename) in
            [ Xtmpl.E (("", "hcode"), [], [Xtmpl.D file_contents]) ]
      in
      let raw_link =
        match kind with
          Unix.S_DIR -> []
        | _ ->
            let href = Rdf_iri.to_uri 
              (Grdfs.iri_input_file_path ~raw: true prefix input file_path)
            in
            [ Xtmpl.D " [";
              a ~href [Xtmpl.D "raw"] ;
              Xtmpl.D "]" ;
            ]
      in
      let title =
        Printf.sprintf "%s%s"
        (match List.rev file_path with [] -> assert false | s :: _ -> s)
        (Xtmpl.string_of_xmls raw_link)
      in
      let wtitle = Rdf_iri.string (Grdfs.iri_input_file_path prefix input file_path) in
      let navpath = xhtml_navpath ctx (`Input_file (input, file_path)) in
      ([ctype ()], in_page ctx ~title ~wtitle ~navpath contents)
;;



let inst_chain_query_of_args args =
  let input =
    match Rest_types.get_arg args "input" with
      "" -> None
    | s ->
        let input = Misc.split_filename s in
        match Rest_types.get_arg args "inputid" with
          "" -> Some (input, None)
        | id -> Some (input, Some id)
  in
  let chain =
    match Rest_types.get_arg args "chain" with
      "" -> None
    | s -> Some (Rdf_iri.iri s)
  in
  let tools =
    let l = Misc.split_string (Rest_types.get_arg args "tools") [','] in
    let f acc version =
      let iri = Rdf_iri.iri version in
      Irimap.add (Grdf_version.tool_of_version iri) iri acc
    in
    List.fold_left f Irimap.empty l
  in
  { Rest_types.iq_chain = chain ;
    iq_input = input ;
    iq_tools = tools ;
  }
;;

let inst_chain_query ctx iq =
  let contents =
    match iq.Rest_types.iq_input, iq.Rest_types.iq_chain, Irimap.is_empty iq.Rest_types.iq_tools with
      None, None, true -> Xtmpl.D "Please give at least one criterium"
    | _ ->
        let inst_list = Chn_inst_query.query_instances ctx
          ?input: iq.Rest_types.iq_input ?chain: iq.Rest_types.iq_chain
          ~tools: iq.Rest_types.iq_tools
        in
        xhtml_inst_list ctx inst_list
  in
  let env = Xtmpl.env_of_list (Rest_xpage.default_commands ctx.ctx_cfg) in
  let xml = Xtmpl.apply_to_xmls env [contents] in
  ([ctype ()], Xtmpl.string_of_xmls xml)
;;

let get_inst_chains ctx args =
  match Rest_types.get_arg args "query" with
    "" ->
      begin
        let title = "Executions" in
        let javascript = Buffer.create 256 in
        let add_js file =
          let file = List.fold_left Fname.concat_s (Config.web_dir ctx.ctx_cfg)
            ["tmpl" ; file]
          in
          Buffer.add_string javascript (Misc.string_of_file (Fname.abs_string file))
        in
        List.iter add_js [ "inst_chain_query.js" ];

        let iq = inst_chain_query_of_args args in
        let input_options _ _ _ =
          let inputs = Ind_io.list_inputs ctx.ctx_cfg in
          let inputs = List.sort Pervasives.compare inputs in
          let selected =
            match iq.Rest_types.iq_input with
              None -> ""
            | Some (i, _) -> String.concat "/" i
          in
          List.map
            (fun i ->
              let atts =
                (("", "value"), Fname.rel_string i) :: (if (Fname.rel_string i) = selected then [("", "selected"),"true"] else [])
              in
              Xtmpl.E (("", "option"), atts, [Xtmpl.D (Fname.rel_string i)]))
            inputs
        in
        let chain_options _ _ _ =
          let prefix = ctx.ctx_cfg.Config.rest_api in
          let selected =
            match iq.Rest_types.iq_chain with
              None -> ""
            | Some iri -> Rdf_iri.string iri
          in
          let f_fchain acc iri =
            match Chn_types.is_iri_fchain ctx iri with
            | None -> acc
            | Some name ->
                match Chn_types.fchain_id name with
                  None -> acc
                | Some id ->
                    let iri = Rdf_iri.string iri in
                    let atts =
                      (("", "value"), iri) ::
                      (if iri = selected then [("", "selected"), "true"] else [])
                    in
                    (Xtmpl.E (("", "option"), atts, [Xtmpl.D ("  "^id)])) :: acc
          in
          let f_chain modname acc chn =
            let name = Chn_types.mk_chain_name modname chn.Chn_ast.chn_name in
            let iri = Chn_types.iri_chain prefix name in
            let acc =
              let iri = Rdf_iri.string iri in
              let atts =
                (("", "value"), iri) ::
                (if iri = selected then [("", "selected"), "true"] else [])
              in
              (Xtmpl.E (("", "option"), atts,
                [Xtmpl.D (Chn_types.string_of_chain_name name)])
              ) :: acc
            in
            let flats = Chn_flat.flat_chains_of_chain ctx name in
            List.fold_left f_fchain acc flats
          in
          let f_mod acc m =
            let file = Chn_io.file_of_modname ctx.ctx_cfg m in
            let chains =
              try
                let m = Chn_io.chn_module_of_file file in
                List.sort
                  (fun c1 c2 -> Pervasives.compare c1.Chn_ast.chn_name c2.Chn_ast.chn_name)
                  m.Chn_ast.cmod_chains
              with _ -> []
            in
            List.fold_left (f_chain m) acc chains
          in
          let chain_files = Chn_io.chain_files ctx.ctx_cfg in
          let modules = List.map Chn_io.modname_of_file chain_files in
          let modules = List.sort Chn_types.compare_chain_modname modules in
          List.rev (List.fold_left f_mod [] modules)
        in
        Buffer.add_string javascript
          "function onPageLoad() {\n  initInputAndChain();\n";

        let tools =
          let result =
            let f_version version =
              let name = Grdf_version.name ctx.ctx_rdf version in
              Xtmpl.E (("", "option"), [("", "value"), Rdf_iri.string version], [Xtmpl.D ("  "^name)])
            in
            let f tool =
              let versions = Grdf_version.versions_of ctx.ctx_rdf ~recur: true tool in
              let tool_name = Grdf_tool.name ctx.ctx_rdf tool in
              let options = List.map f_version versions in
              let id = Printf.sprintf "tool%s" tool_name in
              Printf.bprintf javascript
                "  onToolChange('%s',document.getElementById('%s'));\n"
                tool_name id;
              Xtmpl.E (("", "div"), [("", "class"), "control-group"],
               [
                 Xtmpl.E (("", "label"), [("", "for"), id], [ Xtmpl.D tool_name ]) ;
                 Xtmpl.E (("", "div"), [("", "class"), "controls"],
                  [
                    Xtmpl.E (("", "select"),
                     [("", "name"), id; ("", "id"), id ;
                      ("", "onChange"), Printf.sprintf "onToolChange('%s',this, true)" tool_name],
                     (Xtmpl.E (("", "option"), [("", "value"), ""], [])) :: options
                    )
                  ]
                 )
               ]
              )
            in
            let tools = Grdf_tool.tools ctx.ctx_rdf in
            let tools = List.sort Rdf_iri.compare tools in
            List.map f tools
          in
          fun _ _ _ -> result
        in
        Buffer.add_string javascript "filter();\n}\n";
        let env =
           (("", "input_options"), input_options) ::
           (("", "chain_options"), chain_options) ::
           (("", "tools"), tools) :: []
        in
        let env = Xtmpl.env_of_list env in
        let contents =
          [ Xtmpl.E (("","include"), [("","file"), "inst_chain_filter.tmpl"], []) ]
        in
        ([ctype ()], out_page ctx ~env ~title ~javascript: (Buffer.contents javascript) contents)
      end
  | _ ->
      let iq = inst_chain_query_of_args args in
      inst_chain_query ctx iq
;;

let ichain_digest ctx iri =
  let tmpl_dir = Rest_xpage.tmpl_dir ctx.ctx_cfg in
  let tmpl = Fname.concat_s tmpl_dir "inst_chain_digest.tmpl" in
  let date =
    match Grdfs.creation_date_iri ctx.ctx_rdf iri with
      None -> "??"
    | Some d -> Netdate.mk_mail_date (Netdate.since_epoch d)
  in
  let fchain = xhtml_flat_chain_of_ichain ctx iri in
  let tools = xhtml_tools_of_ichain ctx ~short: true iri in
  let input = xhtml_input_of_ichain ctx iri in
  let env = Xtmpl.env_of_list
    [
     ("","ichain"), (fun _ _ _ -> [Xtmpl.D (Rdf_iri.string iri)]) ;
     ("","date"), (fun _ _ _ -> [Xtmpl.D date ]) ;
     ("","fchain"), (fun _ _ _ -> fchain) ;
     ("","input"), (fun _ _ _ -> input) ;
     ("","tools"), (fun _ _ _ -> tools) ;
    ]
  in
  Xtmpl.apply_to_file env (Fname.abs_string tmpl)
;;

let get_diff_ichains ctx args =
  let get var =
    try
      let s = List.assoc var args in
      Misc.opt_of_string (Misc.strip_string s)
    with Not_found -> None
  in
  let inst1 = get "inst1" in
  let inst2 = get "inst2" in
  let diffcmd = Misc.map_opt Misc.strip_string (get "diffcmd") in
  let diff =
    match inst1, inst2 with
    | Some inst1, Some inst2 ->
        let inst1 = Rdf_iri.iri inst1 in
        let inst2 = Rdf_iri.iri inst2 in
        let diffcmd =
          match diffcmd with
            None -> None
          | Some s ->
              (* ensure this is a predefined diff command, so that a malicious
                 user cannot issue commands like "rm -fr /" or worse ... *)
              let iri = Grdfs.iri_diffcommand ~prefix: ctx.ctx_cfg.Config.rest_api ~name: s in
              Grdf_diff.command_path ctx.ctx_rdf iri
        in
        Chn_diff.diff ctx ~html: true ?diff: diffcmd ~fragment: true inst1 inst2
    | None, _
    | _, None -> "Please give two urls."
  in
  let action =
    let iri = Rdf_iri.concat ctx.ctx_cfg.Config.rest_api Grdfs.suffix_diff in
    let iri = Rdf_iri.concat iri Grdfs.suffix_ichains in
    Rdf_iri.string iri
  in
  let info = function
    None -> []
  | Some iri ->
      let iri = Rdf_iri.iri iri in
      ichain_digest ctx iri
  in
  let digest1 = info inst1 in
  let digest2 = info inst2 in
  let show_form =
    try
      match List.assoc "form" args with
        "false" -> "false"
      | _ -> "true"
    with Not_found -> "true"
  in
  let diffcmd_choices =
    let cmds = Grdf_diff.diffcommands ctx.ctx_rdf in
    let l = Iriset.fold
      (fun iri acc ->
         let name = Grdf_diff.name ctx.ctx_rdf iri in
         match Grdf_diff.command_path ctx.ctx_rdf iri with
           None -> acc
         | Some command ->
             let atts =
               (("","value"), name) ::
               (if diffcmd = Some name then [("","selected"),"true"] else [])
             in
             Xtmpl.E (("","option"), atts, [Xtmpl.D (command^" ("^name^")")]) :: acc
      )
      cmds
      []
    in
    (Xtmpl.E (("","option"), [("","value"), ""], [Xtmpl.D ("<default>")])) :: l
  in
  let env = Xtmpl.env_of_list
    [
     (("", "show-form"), (fun _ _ _ -> [Xtmpl.D show_form])) ;
     (("", "inst1"), (fun _ _ _ -> [Xtmpl.D (Misc.string_of_opt inst1)])) ;
     (("", "inst2"), (fun _ _ _ -> [Xtmpl.D (Misc.string_of_opt inst2)])) ;
     (("", "diffcmd-choices"), (fun _ _ _ -> diffcmd_choices)) ;
     (("", "diffcmd"), (fun _ _ _ -> (match diffcmd  with None -> [] | Some s -> [Xtmpl.D s])));
     (("", "diff"), (fun _ _ _ -> [Xtmpl.xml_of_string diff])) ;
     (("", "action"), (fun _ _ _ -> [Xtmpl.D action])) ;
     (("", "digest1"), (fun _ _ _ -> digest1)) ;
     (("", "digest2"), (fun _ _ _ -> digest2))
    ]
  in
  let contents =
    [ Xtmpl.E (("","include"), [("","file"), "inst_chain_diff.tmpl"], []) ]
  in
  let page = diff_page ctx ~env ~title: "Diffs between instanciated chains" contents in
  ([ctype ()], page)
;;

let get ctx thing args =
  match thing with
  | Other _ -> get_root ctx
  | Static_file (f, t) -> ([ctype ~t ()], Misc.string_of_file f)
  | Tool iri -> get_tool ctx iri
  | Tools -> get_tools ctx
  | Branch iri -> get_branch ctx iri
  | Version iri -> get_version ctx iri
  | Intf iri -> get_intf ctx iri
  | Intfs iri -> get_intfs ctx iri
  | Filetype iri -> get_filetype ctx iri
  | Filetypes -> get_filetypes ctx
  | Versions iri -> get_versions ctx iri
  | Branches iri -> get_branches ctx iri
  | Chains -> get_chains ctx
  | Chain_module modname -> handle_chain_error ctx (get_chain_module ?nav: None ctx) modname
  | Chain fullname -> handle_chain_error ctx (get_chain ctx) fullname
  | Flat_chains -> get_fchains ctx
  | Flat_chain_module modname -> get_fchain_module ctx modname
  | Flat_chain iri -> handle_chain_error ctx (get_fchain ctx) iri
  | Flat_chain_list fchain_name -> get_fchain_list ctx fchain_name
  | Inst_chain iri -> get_ichain ctx iri
  | Out_file (path, raw) -> handle_outfile_error ctx (get_outfile ctx path) raw
  | Inst_producers_of path -> get_inst_producers_of ctx path
  | Inputs -> get_inputs ctx
  | Input path -> handle_in_error ctx (get_input ctx) path
  | Input_file (input, file_path, raw) ->
      handle_in_error ctx (get_input_file ctx ~raw ~input) file_path
  | Inst_chains -> get_inst_chains ctx args
  | Inst_chain_query iq -> inst_chain_query ctx iq
  | Inst_chain_op iri -> handle_chain_error ctx (get_ichain_op ctx) iri
  | Diff_inst_chains -> handle_chain_error ctx (get_diff_ichains ctx) args
(*  | _ -> ([ctype ()], page ctx ~title: "Not implemented" "This page is not implemented yet")*)
;;
