(*********************************************************************************)
(*                Genet                                                          *)
(*                                                                               *)
(*    Copyright (C) 2012 Institut National de Recherche en Informatique          *)
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

open Rdf_node;;
open Rest_types;;

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

let a_by_class ctx uri =
  let cl =
    match Grdfs.class_of ctx.ctx_rdf (Uri uri) with
      None -> ""
    | Some cl -> Grdfs.string_of_class cl
  in
  let name = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api uri in
  [
    Xtmpl.D (match cl with  "" -> "" | _ ->  cl^" ") ;
    a ~href: uri [Xtmpl.D name]
  ]
;;

let a_filetypes ctx =
  a ~href: (Grdfs.uri_filetypes ctx.ctx_cfg.Config.rest_api)
    [ Xtmpl.D Grdfs.suffix_filetypes]
;;

let a_filetype ctx uri =
  let name = Grdf_ftype.name ctx.ctx_rdf uri in
  a ~href: uri [ Xtmpl.D name ]
;;

let a_tools ctx =
  a ~href: (Grdfs.uri_tools ctx.ctx_cfg.Config.rest_api) [Xtmpl.D Grdfs.suffix_tools]
;;

let a_tool ctx uri =
  let name = Grdf_tool.name ctx.ctx_rdf uri in
  a ~href: uri [Xtmpl.D name]
;;

let a_version ctx uri =
  let name = Grdf_version.name ctx.ctx_rdf uri in
  a ~href: uri [Xtmpl.D name]
;;

let a_branch ctx uri =
  let name = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api uri in
  a ~href: uri [Xtmpl.D name]
;;

let a_chain ctx ?(full=false) fullname =
  let href = Chn_types.uri_chain ctx.ctx_cfg.Config.rest_api fullname in
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
  let href = Chn_types.uri_chain_module prefix modname in
  a ~href [Xtmpl.D (Chn_types.string_of_chain_modname modname)]
;;

let a_fchain_module ctx modname =
  let prefix = ctx.ctx_cfg.Config.rest_api in
  let href = Chn_types.uri_fchain_module prefix modname in
  a ~href [Xtmpl.D (Chn_types.string_of_chain_modname modname)]
;;

let a_fchain ctx ?label fullname =
  let prefix = ctx.ctx_cfg.Config.rest_api in
  let href = Chn_types.uri_fchain prefix fullname in
  let label = match label with
      None -> Chn_types.string_of_chain_basename (Chn_types.fchain_basename fullname)
    | Some s -> s
  in
  a ~href [Xtmpl.D label]
;;

let a_ichain ctx name =
  let prefix = ctx.ctx_cfg.Config.rest_api in
  let href = Chn_types.uri_ichain prefix name in
  a ~href [Xtmpl.D (Chn_types.string_of_ichain_name name)]
;;

let a_outfile ctx path =
  match List.rev path with
    [] -> Xtmpl.D ""
  | basename :: _ ->
      let prefix = ctx.ctx_cfg.Config.rest_api in
      let href = Grdfs.uri_outfile_path prefix path in
      a ~href [Xtmpl.D basename]
;;

let a_input ctx ?(full=true) path =
  match List.rev path with
    [] -> Xtmpl.D ""
  | h :: q ->
      let prefix = ctx.ctx_cfg.Config.rest_api in
      let href= Grdfs.uri_input_path prefix path in
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
      let href= Grdfs.uri_input_file_path prefix path file_path in
      let label = match file_path with [] -> assert false | s :: _ -> s in
      a ~href [Xtmpl.D label]
;;

let xhtml_of_ports ctx dir uri =
  let ports = Grdf_port.ports ctx.ctx_rdf uri dir in
  let sep = match dir with Grdf_port.In -> " -&gt; " | Grdf_port.Out -> " * " in
  let f s =
    let uri_ftype = Grdfs.uri_filetype ctx.ctx_cfg.Config.rest_api s in
    Printf.sprintf "<a href=%S>%s</a>" (Rdf_uri.string uri_ftype) s
  in
  [Xtmpl.xml_of_string
    (Grdf_port.string_type_of_ports ctx.ctx_rdf
     (Grdf_port.string_of_port_type f)
     ~sep
     ports
    )
  ]
;;

let xhtml_of_intf_type ctx uri =
  (xhtml_of_ports ctx Grdf_port.In uri) @
  [ Xtmpl.D  " -> " ] @
  (xhtml_of_ports ctx Grdf_port.Out uri)
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

let xhtml_navpath_of_filetype ctx uri =
  xhtml_navpath_join_path [ a_filetypes ctx ]
;;

let navpath_of_branch ctx ?(inc_uri=false) uri =
  let link uri = a ~href: uri [Xtmpl.D (Grdfs.name ctx.ctx_rdf (Uri uri))] in
  let rec iter acc = function
    None -> acc
  | Some (uri, is_tool) ->
      let acc =
        if is_tool then
          (
           let href = Grdfs.uri_branches uri in
           (link uri :: (a ~href [Xtmpl.D Grdfs.suffix_branches]) :: acc)
          )
        else
          link uri :: acc
      in
      iter acc (Grdf_branch.parent ctx.ctx_rdf uri)
  in
  let acc = if inc_uri then [link uri] else [] in
  let p = iter acc (Grdf_branch.parent ctx.ctx_rdf uri) in
  (navpath_of_tool ctx) @ p
;;

let xhtml_navpath_of_branch ctx ?inc_uri uri =
  xhtml_navpath_join_path (navpath_of_branch ctx ?inc_uri uri)
;;

let xhtml_navpath_of_branches ctx uri =
  xhtml_navpath_of_branch ctx ~inc_uri: true uri
;;

let xhtml_navpath_of_version ctx ?inc_uri uri =
  (match Grdf_version.parent ctx.ctx_rdf uri with
     None -> []
   | Some uri -> xhtml_navpath_of_branch ctx ~inc_uri: true uri
  )
;;

let xhtml_navpath_of_versions ctx uri =
  let p = navpath_of_branch ctx ~inc_uri: true uri in
  xhtml_navpath_join_path p
;;

let xhtml_navpath_of_intf ctx uri =
  let tool = Grdf_intf.tool_of_intf uri in
  let uri_intfs = Grdfs.uri_intfs ~tool in
  xhtml_navpath_join_path
  [ a_tools ctx ;
    (a_tool ctx tool) ;
    a ~href: uri_intfs [Xtmpl.D Grdfs.suffix_intfs] ;
  ]
;;

let xhtml_navpath_of_intfs ctx uri =
  if Grdfs.is_a_version ctx.ctx_rdf uri then
    xhtml_navpath_of_version ctx ~inc_uri: true uri
  else
    xhtml_navpath_of_branch ctx ~inc_uri: true uri
;;

let navpath_of_chain_module ctx =
  [
    a ~href: (Grdfs.uri_chains ctx.ctx_cfg.Config.rest_api) [Xtmpl.D Grdfs.suffix_chains] ;
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
    a ~href: (Grdfs.uri_fchains ctx.ctx_cfg.Config.rest_api) [Xtmpl.D Grdfs.suffix_fchains] ;
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

let navpath_of_ichain ctx uri =
  match Chn_inst.instance_source ctx uri with
    None -> []
  | Some uri_fchain ->
      match Chn_types.is_uri_fchain ctx uri_fchain with
        None -> []
      | Some fchain_name -> navpath_of_fchain ctx fchain_name
;;

let xhtml_navpath_of_ichain ctx uri =
  xhtml_navpath_join_path (navpath_of_ichain ctx uri)
;;

let xhtml_navpath_of_ichain_op ctx uri_ichain =
  match Chn_types.is_uri_ichain ctx.ctx_cfg.Config.rest_api uri_ichain with
    None -> []
  | Some name ->
      let p = navpath_of_ichain ctx uri_ichain in
      let id = Chn_types.ichain_id name in
      let path = p @ [a ~href: uri_ichain [Xtmpl.D id]] in
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
        let href = Grdfs.uri_outfile_path ctx.ctx_cfg.Config.rest_api [] in
        a ~href [Xtmpl.D Grdfs.suffix_out]
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
        let href = Grdfs.uri_input_path ctx.ctx_cfg.Config.rest_api [] in
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
| `Tool uri -> xhtml_navpath_of_tool ctx
| `Branch uri -> xhtml_navpath_of_branch ctx uri
| `Version uri -> xhtml_navpath_of_version ctx uri
| `Intf uri -> xhtml_navpath_of_intf ctx uri
| `Intfs uri -> xhtml_navpath_of_intfs ctx uri
| `Filetype uri -> xhtml_navpath_of_filetype ctx uri
| `Filetypes -> []
| `Versions uri -> xhtml_navpath_of_versions ctx uri
| `Branches uri -> xhtml_navpath_of_branches ctx uri
| `Chain_module modname -> xhtml_navpath_of_chain_module ctx
| `Chain fullname -> xhtml_navpath_of_chain ctx fullname
| `Flat_chain_module modname -> xhtml_navpath_of_fchain_module ctx
| `Flat_chain name -> xhtml_navpath_of_fchain ctx name
| `Flat_chain_list fchain_name -> xhtml_navpath_of_fchain_list ctx fchain_name
| `Inst_chain uri -> xhtml_navpath_of_ichain ctx uri
| `Out_file path -> xhtml_navpath_of_outfile ctx path
| `Input path -> xhtml_navpath_of_input ctx path
| `Input_file (input, path) -> xhtml_navpath_of_input_file ctx ~input path
| `Inst_chain_op ichain -> xhtml_navpath_of_ichain_op ctx ichain
;;

let intf_list ctx intfs =
  let heads = [ "Name" ; "Type" ] in
  let f intf =
    [ [ a ~href: intf [Xtmpl.D (Grdf_intf.name ctx.ctx_rdf intf)] ] ;
      [ Xtmpl.E (("","code"), [], xhtml_of_intf_type ctx intf) ];
    ]
  in
  let rows = List.map f intfs in
  [ table ~heads rows ]
;;

let xhtml_of_intfs_of ctx uri =
  prerr_endline "xhtml_of_intfs: start";
  let (explicit, inherited) = Grdf_intf.compute_intfs_of ctx.ctx_rdf uri in
  prerr_endline "xhtml_of_intfs: explicit,inherited ok";
  let intfs = intf_list ctx (Uriset.elements explicit) in
  prerr_endline "xhtml_of_intfs: intfs ok";
  let inherited_intfs = intf_list ctx (Uriset.elements inherited) in
  prerr_endline "xhtml_of_intfs: inherited ok";
  let tmpl = Rest_xpage.tmpl_file ctx.ctx_cfg "intfs.tmpl" in
  let env = Xtmpl.env_of_list
    [
      ("", "has_intfs"), (fun _ _ _ -> [if Uriset.is_empty explicit then Xtmpl.D "" else Xtmpl.D "true"])  ;
      ("", "intfs"), (fun _ _ _ -> intfs) ;
      ("", "has_inherited_intfs"), (fun _ _ _ -> [if Uriset.is_empty inherited then Xtmpl.D "" else Xtmpl.D "true"]) ;
      ("", "inherited_intfs"), (fun _ _ _ -> inherited_intfs) ;
    ]
  in
  let env = Xtmpl.env_of_list ~env (Rest_xpage.default_commands ctx.ctx_cfg) in
  Xtmpl.apply_to_file env tmpl
;;

let xhtml_of_branches_of ctx uri =
  let branches = Grdf_branch.subs ctx.ctx_rdf uri in
  match branches with
    [] -> [Xtmpl.D "No branch."]
  | _ ->
      let heads = ["Branch"] in
      let f br = [ [ a_branch ctx br ] ] in
      let branches = List.sort Rdf_uri.compare branches in
      let rows = List.map f branches in
      [ table ~heads rows ]
;;

let xhtml_of_versions_of ctx uri =
  let versions = Grdf_version.versions_of ctx.ctx_rdf ~recur: true uri in
  let versions = List.sort Rdf_uri.compare versions in
  let heads = ["Active" ; "Version" ; "Date"] in
  let f version = [ [Xtmpl.D ""] ; [a_version ctx version] ; [Xtmpl.D ""] ] in
  let rows = List.map f versions in
  [ table ~heads rows ]
;;

let get_tool ctx uri =
  let name = Grdf_tool.name ctx.ctx_rdf uri in
  let dot = Grdf_dot.dot_of_tool ctx.ctx_rdf uri in
  let svg = dot_to_svg dot in
  let branches = xhtml_of_branches_of ctx uri in
  let intfs = xhtml_of_intfs_of ctx uri in
  let tmpl = Rest_xpage.tmpl_file ctx.ctx_cfg "tool.tmpl" in
  let env = Xtmpl.env_of_list
    [
      ("", "graph"), (fun _ _ _ -> svg) ;
      ("", "branches"), (fun _ _ _ -> branches) ;
      ("", "interfaces"), (fun _ _ _ -> intfs) ;
    ]
  in
  let env = Xtmpl.env_of_list ~env (Rest_xpage.default_commands ctx.ctx_cfg) in
  let contents = Xtmpl.apply_to_file env tmpl in
  let navpath = xhtml_navpath ctx (`Tool uri) in
  ([ctype ()], tool_page ctx ~title: name ~navpath contents)
;;

let get_tools ctx =
  let wld = ctx.ctx_rdf in
  let tools = Grdf_tool.tools wld in
  let f_tool uri =
    let n_versions = List.length (Grdf_version.versions_of wld ~recur: true uri) in
    let href_versions = Grdfs.uri_versions uri in

    let n_branches = List.length (Grdf_branch.subs wld ~recur: false uri) in
    let href_branches = Grdfs.uri_branches uri in

    let n_intfs = Uriset.cardinal (Grdf_intf.intfs_of_tool wld uri) in
    let href_intfs = Grdfs.uri_intfs uri in

    [ [ a ~href: uri [Xtmpl.D (Grdf_tool.name wld uri)] ] ;
      [ a ~href: href_versions [Xtmpl.D (string_of_int n_versions)] ] ;
      [ a ~href: href_branches [Xtmpl.D (string_of_int n_branches)] ] ;
      [ a ~href: href_intfs [Xtmpl.D (string_of_int n_intfs)] ] ;
    ]
  in
  let heads = [ "Name" ; "Versions" ; "Branches" ; "Interfaces" ] in
  let tools = List.sort Rdf_uri.compare tools in
  let rows = List.map f_tool tools in
  let contents = [table ~heads rows] in
  ([ctype ()], tool_page ctx ~title: "Tools" contents)
;;

let get_intf ctx uri =
  let name = Grdf_intf.name ctx.ctx_rdf uri in
  let tool = Grdf_intf.tool_of_intf uri in
  let tool_name = Grdf_tool.name ctx.ctx_rdf tool in
  let wtitle = Printf.sprintf "%s / %s" tool_name name in
  let typ =
    let of_dir label dir =
      Xtmpl.E (("", "p"), [],
       [ Xtmpl.E (("","strong"), [], [Xtmpl.D label]) ;
         Xtmpl.E (("","code"), [],  (xhtml_of_ports ctx dir uri) );
       ])
    in
    [
      of_dir "Input" Grdf_port.In ;
      of_dir "Output" Grdf_port.Out ;
    ]
  in
  let branches_yes =
    match Grdf_intf.implementors ctx.ctx_rdf uri with
      [] -> prerr_endline "empty branches_yes"; []
    | l -> [ ul (List.map (a_by_class ctx) l) ]
  in
  let branches_no =
    match Grdf_intf.not_implementors ctx.ctx_rdf uri with
      [] -> prerr_endline "empty branches_no"; []
    | l -> [ ul (List.map (a_by_class ctx) l) ]
  in
  let tool = a ~href: tool [Xtmpl.D tool_name] in
  let path =
    Misc.string_of_opt (Grdf_intf.command_path ctx.ctx_rdf uri)
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
  let navpath = xhtml_navpath ctx (`Intf uri) in
  let contents = Xtmpl.apply_to_file env tmpl in
  ([ctype ()], tool_page ctx ~title: name ~wtitle ~navpath contents)
;;

let get_intfs ctx uri =
  let pre = "Interfaces of " in
  let name = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api uri in
  let wtitle = Printf.sprintf "%s %s" pre name in
  let title = Printf.sprintf "%s %s" pre (Xtmpl.string_of_xml (a ~href: uri [Xtmpl.D name])) in
  let contents = xhtml_of_intfs_of ctx uri in
  let navpath = xhtml_navpath ctx (`Intfs uri) in
  ([ctype ()], tool_page ctx ~title ~wtitle ~navpath contents)
;;

let get_filetype ctx uri =
  let name = Grdf_ftype.name ctx.ctx_rdf uri in
  let contents = [Xtmpl.D name] in
  let navpath = xhtml_navpath ctx (`Filetype uri) in
  ([ctype ()], filetype_page ctx ~title: name ~navpath contents)
;;

let get_filetypes ctx =
  let ftypes = Grdf_ftype.filetypes ctx.ctx_rdf in
  let heads = [ "Name" ; "Extension" ; "Description" ] in
  let wld = ctx.ctx_rdf in
  let f uri =
    [ [ a_filetype ctx uri ];
      [ Xtmpl.D (Grdf_ftype.extension wld uri) ] ;
      [ Xtmpl.D (Grdf_ftype.desc wld uri) ];
    ]
  in
  let rows = List.map f ftypes in
  let contents = [ table ~heads rows ] in
  ([ctype ()], filetype_page ctx ~title: "Filetypes" contents)
;;

let get_version ctx uri =
  let wtitle = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api uri in
  let name = Grdf_version.name ctx.ctx_rdf uri in
  let title = Xtmpl.string_of_xml (a ~href: uri [Xtmpl.D name]) in
  let contents = xhtml_of_intfs_of ctx uri in
  let navpath = xhtml_navpath ctx (`Version uri) in
  ([ctype ()], tool_page ctx ~title ~wtitle ~navpath contents)
;;

let get_versions ctx tool =
  let contents = xhtml_of_versions_of ctx tool in
  let title = Printf.sprintf "Versions of %s" (Grdf_tool.name ctx.ctx_rdf tool) in
  let navpath = xhtml_navpath ctx (`Versions tool) in
  ([ctype ()], tool_page ctx ~title ~navpath contents)
;;

let get_branch ctx uri =
  prerr_endline "get_branch: start";
  let name = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api uri in
  prerr_endline "get_branch: name ok";
  let dot = Grdf_dot.dot_of_branch ctx.ctx_rdf uri in
  let svg = dot_to_svg dot in
  prerr_endline "get_branch: svg ok";
  let branches = xhtml_of_branches_of ctx uri in
  prerr_endline "get_branch: branches ok";
  let intfs = xhtml_of_intfs_of ctx uri in
  prerr_endline "get_branch: intfs ok";
  let versions = xhtml_of_versions_of ctx uri in
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
  let contents = Xtmpl.apply_to_file env tmpl in
  let navpath = xhtml_navpath ctx (`Branch uri) in
  ([ctype ()], tool_page ctx ~title: name ~navpath contents)
;;

let get_branches ctx uri =
  let pre = "Branches of " in
  let name = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api uri in
  let wtitle = Printf.sprintf "%s %s" pre name in
  let title = Printf.sprintf "%s %s" pre (Xtmpl.string_of_xml (a ~href: uri [Xtmpl.D name])) in
  let contents = xhtml_of_branches_of ctx uri in
  let navpath = xhtml_navpath ctx (`Branches uri) in
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
       let href = Chn_types.uri_fchain config.Config.rest_api
         (Chn_types.mk_fchain_name name "")
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
      Xtmpl.E (("", "section"), [("","title"), "Source from "^(Filename.quote file)], code) ;
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
    (fun uri ->
       let id =
         match Chn_types.is_uri_fchain ctx uri with
           None -> []
         | Some name ->
             match Chn_types.fchain_id name with
               None -> []
             | Some id -> [a ~href: uri [Xtmpl.D id]]
       in
       let date = Misc.map_opt Netdate.since_epoch (Grdfs.creation_date_uri ctx.ctx_rdf uri) in
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

let get_fchain ctx uri =
  let fchain_name =
    match Chn_types.is_uri_fchain ctx uri with
      None -> failwith (Printf.sprintf "Unknown flat chain %S" (Rdf_uri.string uri))
    | Some n -> n
  in
  let module_ids =
    let heads = [ "Module" ; "Commit id" ] in
    let versions = Chn_flat.fchain_chain_versions ctx uri in
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
(*  let uri_fchain = Chn_types.uri_fchain ctx.ctx_cfg.Config.rest_api fchain_name in*)
  let date = Misc.string_of_opt (Chn_flat.fchain_creation_date ctx uri) in
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
    ] @ svg
  in
  ([ctype ()], chain_page ctx ~title ~wtitle ~navpath contents)
;;

let get_ichain_op ctx uri =
  let inst_chain =
    match Grdfs.subject_uri ctx.ctx_rdf
      ~pred: Grdfs.genet_containsop ~obj: (Uri uri)
    with
      None -> failwith (Printf.sprintf "%S is not contained" (Rdf_uri.string uri))
    | Some u -> u
  in
  let ichain_name =
    match Chn_types.is_uri_ichain ctx.ctx_cfg.Config.rest_api inst_chain with
      None -> assert false
    | Some n -> n
  in
  let op_name = Chn_types.ichain_op_name ~ichain: inst_chain uri in
  let start_date = Misc.string_of_opt (Chn_run.ichain_start_date ctx uri) in
  let stop_date = Misc.string_of_opt (Chn_run.ichain_stop_date ctx uri) in
  let title = op_name in
  let wtitle = Printf.sprintf "%s/%s" (Chn_types.string_of_ichain_name ichain_name) op_name in
  let navpath = xhtml_navpath ctx (`Inst_chain_op inst_chain) in
  let uri_from = Chn_flat.get_op_origin ctx uri in
  let interface =
    match Grdf_intf.intf_exists ctx.Chn_types.ctx_rdf uri_from with
      None -> []
    | Some name ->
        let tool = Grdf_intf.tool_of_intf uri_from in
        let name = Printf.sprintf "%s / %s" (Grdf_tool.name ctx.Chn_types.ctx_rdf tool) name in
        [
          Xtmpl.E (("", "p"), [],
           [ Xtmpl.E (("","strong"), [], [ Xtmpl.D "Interface:" ]);
             a ~href: uri_from [Xtmpl.D name] ;
           ]) ;
        ]
  in
  let return_code =
    match Chn_run.return_code ctx uri with
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
      ~sub: (Rdf_node.Uri uri) ~pred: Grdfs.genet_commandoutput
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
    let dot = Rest_xpage.dot_of_ichain_op ctx uri in
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

let get_ichain ctx uri =
  let ichain_name =
    match Chn_types.is_uri_ichain ctx.ctx_cfg.Config.rest_api uri with
      None -> failwith (Printf.sprintf "Not an instanciated chain uri: %S" (Rdf_uri.string uri))
    | Some n -> n
  in
  let title = "" in
  let wtitle = Chn_types.string_of_ichain_name ichain_name in
  let navpath = xhtml_navpath ctx (`Inst_chain uri) in
  let svg =
    let dot = Rest_xpage.dot_of_ichain ctx ichain_name in
    dot_to_svg ~svg_h: 600 dot
  in
  let date = Misc.string_of_opt (Chn_flat.fchain_creation_date ctx uri) in
  let start_date = Misc.string_of_opt (Chn_run.ichain_start_date ctx uri) in
  let stop_date = Misc.string_of_opt (Chn_run.ichain_stop_date ctx uri) in
  let tool_versions =
    let map = Chn_inst.inst_versions ctx uri in
    let l = Urimap.fold (fun tool v acc -> (tool, v) :: acc) map [] in
    let heads = ["Tool" ; "Version"] in
    let rows = List.map
      (fun (tool, version) ->
        [ [ a_tool ctx tool ] ; [ a_version ctx version ] ]
      )
      l
    in
    [ table ~heads rows ]
  in
  let input_info =
    match Chn_inst.inst_input ctx uri with
      None -> [Xtmpl.D "??"]
    | Some (name, id) ->
        [ Xtmpl.D (Printf.sprintf "%s [%s]" name id) ]
  in
  let flat_uri =
    match Chn_inst.instance_source ctx uri with
      None -> [Xtmpl.D "?"]
    | Some uri ->
        match Chn_types.is_uri_fchain ctx uri with
          None -> [Xtmpl.D "?"]
        | Some flat_name ->
            let chain_name = Chn_types.fchain_chainname flat_name in
            let chain = Chn_types.string_of_chain_name chain_name in
            let label = Misc.string_of_opt (Chn_types.fchain_id flat_name) in
            let fchain = a_fchain ctx ~label flat_name in
            [ Xtmpl.D chain ; Xtmpl.D " [" ; fchain ; Xtmpl.D "]" ]
  in
  let exec_error =
    match Grdfs.object_uri ctx.ctx_rdf
      ~sub: (Rdf_node.Uri uri) ~pred: Grdfs.genet_failedcommand
    with
      None -> []
    | Some err_uri ->
        let s_uri = Rdf_uri.string err_uri in
        [ Xtmpl.E (("","p"), [("","class"), "alert alert-error"],
           [ Xtmpl.D "Failed while running " ;
             Xtmpl.E (("","tt"), [],
               [ a ~href: err_uri [Xtmpl.D s_uri] ])
           ]
          )
        ]
  in
  let log =
    match Grdfs.command_output ctx.ctx_rdf uri with
      None -> []
    | Some log -> [ Xtmpl.E (("","pre"), [], [Xtmpl.D log]) ]
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
       ( (Xtmpl.E (("","strong"), [], [Xtmpl.D "Flat chain:"])) :: flat_uri)
      );
    ]
    @ exec_error
    @ tool_versions
    @svg
    @log
  in
  ([ctype ()], chain_page ctx ~title ~wtitle ~navpath contents)
;;

let get_outfile_raw ctx path =
  let filename =
    List.fold_left Filename.concat (Config.out_dir ctx.ctx_cfg) path
  in
  let file_contents = Misc.string_of_file filename in
  let ctype = ctype ~t: (Misc.file_mimetype filename) () in
  ([ctype], file_contents)
;;

let file_date file =
  try Netdate.mk_mail_date (Unix.stat file).Unix.st_mtime
  with Unix.Unix_error _ -> ""
;;

let xhtml_dir_contents f_uri dir =
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
          let href = f_uri base in
          [ [ a ~href [Xtmpl.D base] ] ] :: acc
         )
    )
    []
    entries
  in
  table rows
;;

let xhtml_outdir_contents ctx path =
  let dir = List.fold_left Filename.concat (Config.out_dir ctx.ctx_cfg) path in
  let prefix = ctx.ctx_cfg.Config.rest_api in
  let f_uri basename =
    Grdfs.uri_outfile_path ~raw: false prefix (path@[basename])
  in
  xhtml_dir_contents f_uri dir
;;

let xhtml_inputdir_contents ctx input path =
  let dir =
    List.fold_left Filename.concat (Config.data_dir ctx.ctx_cfg) (input @ path)
  in
  let prefix = ctx.ctx_cfg.Config.rest_api in
  let f_uri basename =
    Grdfs.uri_input_file_path ~raw: false prefix input (path@[basename])
  in
  xhtml_dir_contents f_uri dir
;;

let get_outfile ctx path raw =
  match raw with
    true -> get_outfile_raw ctx path
  | false ->
      let prefix = ctx.ctx_cfg.Config.rest_api in
      let filename =
        List.fold_left Filename.concat (Config.out_dir ctx.ctx_cfg) path
      in
      let wtitle = String.concat "/" path in
      let kind = (Unix.lstat filename).Unix.st_kind in
      let raw_link =
        match kind with
          Unix.S_DIR -> []
        | _ ->
            let href = Grdfs.uri_outfile_path ~raw: true prefix path in
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
            let file_contents = Misc.string_of_file filename in
            Xtmpl.E (("", "hcode"), [], [Xtmpl.D file_contents])
      in
      let contents =
        [ Xtmpl.E (("","p"), [],
           [ Xtmpl.E (("","strong"), [], [ Xtmpl.D "Date:" ] ) ;
             Xtmpl.D  (file_date filename) ;
           ]) ;
          Xtmpl.E (("","p"), [],
           [ Xtmpl.E (("","div"), [("","class"), "btn-group"],
              [ Xtmpl.E (("","a"),
                 [(("","class"), "btn") ;
                   (("","href"), Rdf_uri.string (Grdfs.uri_ichains_producers_of prefix path))
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
            match Grdfs.name ctx.ctx_rdf (Rdf_node.Uri origin) with
              "" -> pname
            | s -> Printf.sprintf "%s.%s" s pname
          in
          let inst =
            match Grdfs.subject_uri ctx.ctx_rdf
              ~pred: Grdfs.genet_containsop ~obj: (Rdf_node.Uri container)
            with
              None ->
                failwith
                (Printf.sprintf "No instanciated chain ??? (container=%S)" (Rdf_uri.string  container))
            | Some inst -> inst
          in
          Some (name, inst)
    in
    match v with
      None -> acc
    | Some (port_name, inst) ->
        let a_inst =
          match Chn_types.is_uri_ichain ctx.ctx_cfg.Config.rest_api inst with
            None -> Xtmpl.D (Printf.sprintf "%S is not an inst. chain" (Rdf_uri.string inst))
          | Some uri -> a_ichain ctx uri
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
      let ports = Grdfs.subject_uris ctx.ctx_rdf
        ~pred: Grdfs.genet_filemd5
        ~obj: (Rdf_node.node_of_literal_string md5)
      in
      let table = xhtml_inst_list_of_ports ctx ports in
      let title = Printf.sprintf "Producers of %s" (Xtmpl.string_of_xml (a_outfile ctx [md5])) in
      let wtitle = Printf.sprintf "Producers of %s" md5 in
      ([ctype ()], chain_page ctx ~title ~wtitle table)
;;

let get_inputs ctx =
  let inputs = Ind_io.list_inputs ctx.ctx_cfg in
  let heads = ["Input"] in
  let rows = List.map (fun s -> [ [a_input ctx [s]]] ) inputs in
  let table = [ table ~heads rows ] in
  let title = "Inputs" in
  ([ctype ()], in_page ctx ~title table)
;;

let get_input ctx path =
  let dirname =
    List.fold_left Filename.concat (Config.data_dir ctx.ctx_cfg) path
  in
  let title = String.concat "/" path in
  let spec = Ind_io.load ctx.ctx_cfg dirname in
  let git_id = Misc.get_git_id dirname in
  let in_table =
    let f (name, _) =
      let file_path = Misc.split_filename name in
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
    List.fold_left Filename.concat (Config.data_dir ctx.ctx_cfg) (input @ file_path)
  in
  match raw with
    true ->
      let file_contents = Misc.string_of_file filename in
      let ctype = ctype ~t: (Misc.file_mimetype filename) () in
      ([ctype], file_contents)
  | false ->
      let prefix = ctx.ctx_cfg.Config.rest_api in
      let kind = (Unix.lstat filename).Unix.st_kind in
      let contents =
        match kind with
          Unix.S_DIR ->
            [ xhtml_inputdir_contents ctx input file_path ]
        | _ ->
            let file_contents = Misc.string_of_file filename in
            [ Xtmpl.E (("", "hcode"), [], [Xtmpl.D file_contents]) ]
      in
      let raw_link =
        match kind with
          Unix.S_DIR -> []
        | _ ->
            let href = Grdfs.uri_input_file_path ~raw: true prefix input file_path in
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
      let wtitle = Rdf_uri.string (Grdfs.uri_input_file_path prefix input file_path) in
      let navpath = xhtml_navpath ctx (`Input_file (input, file_path)) in
      ([ctype ()], in_page ctx ~title ~wtitle ~navpath contents)
;;

let xhtml_inst_list ctx list =
  let heads = [ "Execution" ; "Date" ] in
  let list = List.map
    (fun uri -> (uri, Grdfs.creation_date_uri ctx.ctx_rdf uri)) list
  in
  let list = List.sort
    (fun (_,d1) (_,d2) -> Pervasives.compare d2 d1) list
  in
  let rows = List.map
    (fun (uri_inst, date) ->
       match Chn_types.is_uri_ichain ctx.ctx_cfg.Config.rest_api uri_inst with
         None -> [ [Xtmpl.D ("bad ichain "^(Rdf_uri.string uri_inst))] ; [Xtmpl.D ""] ]
       | Some name ->
           let date =
             match date with
               None -> ""
             | Some d -> Netdate.mk_mail_date (Netdate.since_epoch d)
           in
           [ [a_ichain ctx name] ; [Xtmpl.D date]]
    )
    list
  in
  table ~heads rows
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
    | s -> Some (Rdf_uri.uri s)
  in
  let tools =
    let l = Misc.split_string (Rest_types.get_arg args "tools") [','] in
    let f acc version =
      let uri = Rdf_uri.uri version in
      Urimap.add (Grdf_version.tool_of_version uri) uri acc
    in
    List.fold_left f Urimap.empty l
  in
  { Rest_types.iq_chain = chain ;
    iq_input = input ;
    iq_tools = tools ;
  }
;;

let inst_chain_query ctx iq =
  let contents =
    match iq.Rest_types.iq_input, iq.Rest_types.iq_chain, Urimap.is_empty iq.Rest_types.iq_tools with
      None, None, true -> Xtmpl.D "Please give at least one criterium"
    | _ ->
        let inst_list = Chn_inst_query.query_instances ctx
          ?input: iq.Rest_types.iq_input ?chain: iq.Rest_types.iq_chain
          ~tools: iq.Rest_types.iq_tools
        in
        xhtml_inst_list ctx inst_list
  in
  ([ctype ()], Xtmpl.string_of_xml contents)
;;

let get_inst_chains ctx args =
  match Rest_types.get_arg args "query" with
    "" ->
      begin
        let title = "Executions" in
        let javascript = Buffer.create 256 in
        let file = List.fold_left Filename.concat (Config.web_dir ctx.ctx_cfg)
          ["tmpl" ; "inst_chain_query.js"]
        in
        Buffer.add_string javascript (Misc.string_of_file file);
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
                (("", "value"), i) :: (if i = selected then [("", "selected"),"true"] else [])
              in
              Xtmpl.E (("", "option"), atts, [Xtmpl.D i]))
            inputs
        in
        let chain_options _ _ _ =
          let prefix = ctx.ctx_cfg.Config.rest_api in
          let selected =
            match iq.Rest_types.iq_chain with
              None -> ""
            | Some uri -> Rdf_uri.string uri
          in
          let f_fchain acc uri =
            match Chn_types.is_uri_fchain ctx uri with
            | None -> acc
            | Some name ->
                match Chn_types.fchain_id name with
                  None -> acc
                | Some id ->
                    let uri = Rdf_uri.string uri in
                    let atts =
                      (("", "value"), uri) ::
                      (if uri = selected then [("", "selected"), "true"] else [])
                    in
                    (Xtmpl.E (("", "option"), atts, [Xtmpl.D ("  "^id)])) :: acc
          in
          let f_chain modname acc chn =
            let name = Chn_types.mk_chain_name modname chn.Chn_ast.chn_name in
            let uri = Chn_types.uri_chain prefix name in
            let acc =
              let uri = Rdf_uri.string uri in
              let atts =
                (("", "value"), uri) ::
                (if uri = selected then [("", "selected"), "true"] else [])
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
              Xtmpl.E (("", "option"), [("", "value"), Rdf_uri.string version], [Xtmpl.D ("  "^name)])
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
            let tools = List.sort Rdf_uri.compare tools in
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

let get ctx thing args =
  match thing with
  | Other _ -> get_root ctx
  | Static_file (f, t) -> ([ctype ~t ()], Misc.string_of_file f)
  | Tool uri -> get_tool ctx uri
  | Tools -> get_tools ctx
  | Branch uri -> get_branch ctx uri
  | Version uri -> get_version ctx uri
  | Intf uri -> get_intf ctx uri
  | Intfs uri -> get_intfs ctx uri
  | Filetype uri -> get_filetype ctx uri
  | Filetypes -> get_filetypes ctx
  | Versions uri -> get_versions ctx uri
  | Branches uri -> get_branches ctx uri
  | Chains -> get_chains ctx
  | Chain_module modname -> handle_chain_error ctx (get_chain_module ?nav: None ctx) modname
  | Chain fullname -> handle_chain_error ctx (get_chain ctx) fullname
  | Flat_chains -> get_fchains ctx
  | Flat_chain_module modname -> get_fchain_module ctx modname
  | Flat_chain uri -> handle_chain_error ctx (get_fchain ctx) uri
  | Flat_chain_list fchain_name -> get_fchain_list ctx fchain_name
  | Inst_chain uri -> get_ichain ctx uri
  | Out_file (path, raw) -> handle_outfile_error ctx (get_outfile ctx path) raw
  | Inst_producers_of path -> get_inst_producers_of ctx path
  | Inputs -> get_inputs ctx
  | Input path -> handle_in_error ctx (get_input ctx) path
  | Input_file (input, file_path, raw) ->
      handle_in_error ctx (get_input_file ctx ~raw ~input) file_path
  | Inst_chains -> get_inst_chains ctx args
  | Inst_chain_query iq -> inst_chain_query ctx iq
  | Inst_chain_op uri -> handle_chain_error ctx(get_ichain_op ctx) uri
(*  | _ -> ([ctype ()], page ctx ~title: "Not implemented" "This page is not implemented yet")*)
;;
