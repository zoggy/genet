(** Replying to xhtml queries. *)

open Rest_types;;

let svg_width = 700;;
let svg_height = 200;;
let svg_dpi = 96. ;;

let dot_to_svg dot =
  let w = (float svg_width) /. svg_dpi in
  let h = (float svg_height) /. svg_dpi in
  let options = Printf.sprintf "-Gfontsize=8. -Gdpi=%.2f -Gsize=\"%.0f,%.0f!\"" svg_dpi w h in
  Grdf_dot.dot_to_svg ~options ~size: (svg_width,svg_height) dot
;;

let ctype ?(t="text/html; charset=\"utf-8\"") () = ("Content-Type", t);;

let page ?(env=Xtmpl.env_empty) ctx ~title ?wtitle ?navpath ?error contents =
  let s = Rest_xpage.page ctx.ctx_cfg ~env
    ~title ?wtitle ?navpath ?error [Xtmpl.xml_of_string contents]
  in
  Printf.sprintf "<!DOCTYPE html>\n%s\n" s
;;

let page_active ?(env=Xtmpl.env_empty) v =
  let env = Xtmpl.env_add_att ("navbar-"^v) "active" env in
  page ~env
;;

let home_page = page_active "home";;
let tool_page = page_active "tools";;
let filetype_page = page_active "filetypes";;
let chain_page = page_active "chains";;

let table ?heads rows =
  let b = Buffer.create 256 in
  Buffer.add_string b "<table class=\"table table-bordered table-striped\">";
  begin
    match heads with
      None -> ()
    | Some heads ->
      Buffer.add_string b "<thead><tr>";
      List.iter (fun s -> Printf.bprintf b "<th>%s</th>" s) heads;
      Buffer.add_string b "</tr></thead>";
  end;
  let f s = Printf.bprintf b "<td>%s</td>" s in
  let f_row l =
    Printf.bprintf b "<tr>"; List.iter f l; Printf.bprintf b "</tr>"
  in
  List.iter f_row rows;
  Buffer.add_string b "</table>";
  Buffer.contents b
;;

let ul l =
  let l = List.map (fun s -> Printf.sprintf "<li>%s</li>" s) l in
  Printf.sprintf "<ul>%s</ul>" (String.concat "" l)
;;

let a ~href contents =
  Xtmpl.string_of_xml
  (Xtmpl.T ("a", ["href", href], [Xtmpl.xml_of_string contents]))
;;

let a_by_class ctx uri =
  let cl =
    match Grdfs.class_of_uri_string ctx.ctx_rdf uri with
      None -> ""
    | Some cl -> Grdfs.string_of_class cl
  in
  let name = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api uri in
  Printf.sprintf "%s%s"
  (match cl with  "" -> "" | _ ->  cl^" ")
  (a ~href: uri name)
;;

let a_filetypes ctx =
  a ~href: (Grdfs.uri_filetypes ctx.ctx_cfg.Config.rest_api) Grdfs.suffix_filetypes
;;

let a_filetype ctx uri =
  let name = Grdf_ftype.name ctx.ctx_rdf uri in
  a ~href: uri name
;;

let a_tools ctx =
  a ~href: (Grdfs.uri_tools ctx.ctx_cfg.Config.rest_api) Grdfs.suffix_tools
;;

let a_tool ctx uri =
  let name = Grdf_tool.name ctx.ctx_rdf uri in
  a ~href: uri name
;;

let a_version ctx uri =
  let name = Grdf_version.name ctx.ctx_rdf uri in
  a ~href: uri name
;;

let a_branch ctx uri =
  let name = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api uri in
  a ~href: uri name
;;

let a_chain ctx fullname =
  let href = Chn_types.uri_chain ctx.ctx_cfg.Config.rest_api fullname in
  let name = Chn_types.string_of_chain_basename (Chn_types.chain_basename fullname) in
  a ~href name
;;

let a_chain_module ctx modname =
  let prefix = ctx.ctx_cfg.Config.rest_api in
  let href = Chn_types.uri_chain_module prefix modname in
  a ~href (Chn_types.string_of_chain_modname modname)
;;

let xhtml_of_ports ctx dir uri =
  let ports = Grdf_port.ports ctx.ctx_rdf uri dir in
  let ports = Grdf_port.sort_ports ports in
  let of_port p =
    let ftype = Grdf_port.port_type ctx.ctx_rdf p in
    match ftype with
      Grdf_port.One uri -> a_filetype ctx uri
    | Grdf_port.List uri -> Printf.sprintf "%s list" (a_filetype ctx uri)
  in
  match ports with
    [] -> "()"
  | _ ->
      let sep = match dir with Grdf_port.In -> " -&gt; " | Grdf_port.Out -> " * " in
      String.concat sep (List.map of_port ports)
;;

let xhtml_of_intf_type ctx uri =
  Printf.sprintf "%s -&gt; %s"
    (xhtml_of_ports ctx Grdf_port.In uri)
    (xhtml_of_ports ctx Grdf_port.Out uri)
;;

let xhtml_navpath_join_path path =
  match path with
    [] -> ""
  | _ -> (String.concat " / " ("" :: path)) ^ " /"
;;

let navpath_of_tool ctx = [ a_tools ctx ];;

let xhtml_navpath_of_tool ctx =
  xhtml_navpath_join_path (navpath_of_tool ctx)
;;

let xhtml_navpath_of_filetype ctx uri =
  xhtml_navpath_join_path [ a_filetypes ctx ]
;;

let navpath_of_branch ctx ?(inc_uri=false) uri =
  let link uri = a ~href: uri (Grdfs.name_of_uri_string ctx.ctx_rdf uri) in
  let rec iter acc = function
    None -> acc
  | Some (uri, is_tool) ->
      let acc =
        if is_tool then
          (
           let href = Grdfs.uri_branches uri in
           (link uri :: (a ~href Grdfs.suffix_branches) :: acc)
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
     None -> ""
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
    a ~href: uri_intfs Grdfs.suffix_intfs ;
  ]
;;

let xhtml_navpath_of_intfs ctx uri =
  let node = Rdf_node.new_from_uri_string ctx.ctx_rdf.Grdf_types.wld_world uri in
  if Grdfs.is_a_version ctx.ctx_rdf node then
    xhtml_navpath_of_version ctx ~inc_uri: true uri
  else
    xhtml_navpath_of_branch ctx ~inc_uri: true uri
;;

let navpath_of_chain_module ctx =
  [
    a ~href: (Grdfs.uri_chains ctx.ctx_cfg.Config.rest_api) Grdfs.suffix_chains ;
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

let xhtml_navpath ctx = function
| Other _
| Static_file _
| Chains
| Tools -> ""
| Tool uri -> xhtml_navpath_of_tool ctx
| Branch uri -> xhtml_navpath_of_branch ctx uri
| Version uri -> xhtml_navpath_of_version ctx uri
| Intf uri -> xhtml_navpath_of_intf ctx uri
| Intfs uri -> xhtml_navpath_of_intfs ctx uri
| Filetype uri -> xhtml_navpath_of_filetype ctx uri
| Filetypes -> ""
| Versions uri -> xhtml_navpath_of_versions ctx uri
| Branches uri -> xhtml_navpath_of_branches ctx uri
| Chain_module modname -> xhtml_navpath_of_chain_module ctx
| Chain fullname -> xhtml_navpath_of_chain ctx fullname
;;

let intf_list ctx intfs =
  let heads = [ "Name" ; "Type" ] in
  let f intf =
    [ a ~href: intf (Grdf_intf.name ctx.ctx_rdf intf) ;
      Printf.sprintf "<code>%s</code>" (xhtml_of_intf_type ctx intf) ;
    ]
  in
  let rows = List.map f intfs in
  table ~heads rows
;;

let xhtml_of_intfs_of ctx uri =
  let (explicit, inherited) = Grdf_intf.compute_intfs_of ctx.ctx_rdf uri in
  let intfs = intf_list ctx (Sset.elements explicit) in
  let inherited_intfs = intf_list ctx (Sset.elements inherited) in
  let tmpl = Rest_xpage.tmpl_file ctx.ctx_cfg "intfs.tmpl" in
  let env = List.fold_left
    (fun e (name, v) -> Xtmpl.env_add_att name v e)
    Xtmpl.env_empty
    [
      "has_intfs", (if Sset.is_empty explicit then "" else "true") ;
      "intfs", intfs ;
      "has_inherited_intfs", (if Sset.is_empty inherited then "" else "true") ;
      "inherited_intfs", inherited_intfs ;
    ]
  in
  let env = Xtmpl.env_of_list ~env (Rest_xpage.default_commands ctx.ctx_cfg) in
  Xtmpl.apply_from_file env tmpl
;;

let xhtml_of_branches_of ctx uri =
  let branches = Grdf_branch.subs ctx.ctx_rdf uri in
  match branches with
    [] -> "No branch."
  | _ ->
      let heads = ["Branch"] in
      let f br = [a_branch ctx br] in
      let rows = List.sort Pervasives.compare (List.map f branches) in
      table ~heads rows
;;

let xhtml_of_versions_of ctx uri =
  let versions = Grdf_version.versions_of ctx.ctx_rdf ~recur: true uri in
  let heads = ["Active" ; "Version" ; "Date"] in
  let f version = ["" ; a_version ctx version ; ""] in
  let rows = List.sort Pervasives.compare (List.map f versions) in
  table ~heads rows
;;

let get_tool ctx uri =
  let name = Grdf_tool.name ctx.ctx_rdf uri in
  let dot = Grdf_dot.dot_of_tool ctx.ctx_rdf uri in
  let svg = dot_to_svg dot in
  let branches = xhtml_of_branches_of ctx uri in
  let intfs = xhtml_of_intfs_of ctx uri in
  let tmpl = Rest_xpage.tmpl_file ctx.ctx_cfg "tool.tmpl" in
  let env = List.fold_left
    (fun e (name, v) -> Xtmpl.env_add_att name v e)
    Xtmpl.env_empty
    [
      "graph", svg ;
      "branches", branches ;
      "interfaces", intfs ;
    ]
  in
  let env = Xtmpl.env_of_list ~env (Rest_xpage.default_commands ctx.ctx_cfg) in
  let contents = Xtmpl.apply_from_file env tmpl in
  let navpath = xhtml_navpath ctx (Tool uri) in
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

    let n_intfs = Sset.cardinal (Grdf_intf.intfs_of_tool wld uri) in
    let href_intfs = Grdfs.uri_intfs uri in

    [ a ~href: uri (Grdf_tool.name wld uri) ;
      a ~href: href_versions (string_of_int n_versions) ;
      a ~href: href_branches (string_of_int n_branches);
      a ~href: href_intfs (string_of_int n_intfs) ;
    ]
  in
  let heads = [ "Name" ; "Versions" ; "Branches" ; "Interfaces" ] in
  let rows = List.sort Pervasives.compare (List.map f_tool tools) in
  let contents = table ~heads rows in
  ([ctype ()], tool_page ctx ~title: "Tools" contents)
;;

let get_intf ctx uri =
  let name = Grdf_intf.name ctx.ctx_rdf uri in
  let tool = Grdf_intf.tool_of_intf uri in
  let tool_name = Grdf_tool.name ctx.ctx_rdf tool in
  let wtitle = Printf.sprintf "%s / %s" tool_name name in
  let typ =
    let of_dir label dir =
      Printf.sprintf "<p><strong>%s:</strong> <code> %s </code></p>"
        label (xhtml_of_ports ctx dir uri)
    in
    Printf.sprintf "%s%s"
    (of_dir "Input" Grdf_port.In)
    (of_dir "Output" Grdf_port.Out)
  in
  let branches_yes =
    match Grdf_intf.implementors ctx.ctx_rdf uri with
      [] -> prerr_endline "empty branches_yes"; ""
    | l -> ul (List.map (a_by_class ctx) l)
  in
  let branches_no =
    match Grdf_intf.not_implementors ctx.ctx_rdf uri with
      [] -> prerr_endline "empty branches_no"; ""
    | l -> ul (List.map (a_by_class ctx) l)
  in
  let tool = a ~href: tool tool_name in
  let path =
    Misc.string_of_opt (Grdf_intf.command_path ctx.ctx_rdf uri)
  in
  let tmpl = Rest_xpage.tmpl_file ctx.ctx_cfg "intf.tmpl" in
  let env = List.fold_left
    (fun e (name, v) -> Xtmpl.env_add_att name v e)
    Xtmpl.env_empty
    [
      "type", typ ;
      "path", path ;
      "tool", tool ;
      "branches_yes", branches_yes ;
      "branches_no", branches_no ;
    ]
  in
  let env = Xtmpl.env_of_list ~env (Rest_xpage.default_commands ctx.ctx_cfg) in
  let navpath = xhtml_navpath ctx (Intf uri) in
  let contents = Xtmpl.apply_from_file env tmpl in
  ([ctype ()], tool_page ctx ~title: name ~wtitle ~navpath contents)
;;

let get_intfs ctx uri =
  let pre = "Interfaces of " in
  let name = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api uri in
  let wtitle = Printf.sprintf "%s %s" pre name in
  let title = Printf.sprintf "%s %s" pre (a ~href: uri name) in
  let contents = xhtml_of_intfs_of ctx uri in
  let navpath = xhtml_navpath ctx (Intfs uri) in
  ([ctype ()], tool_page ctx ~title ~wtitle ~navpath contents)
;;

let get_filetype ctx uri =
  let name = Grdf_ftype.name ctx.ctx_rdf uri in
  let contents = name in
  let navpath = xhtml_navpath ctx (Filetype uri) in
  ([ctype ()], filetype_page ctx ~title: name ~navpath contents)
;;

let get_filetypes ctx =
  let ftypes = Grdf_ftype.filetypes ctx.ctx_rdf in
  let heads = [ "Name" ; "Extension" ; "Description" ] in
  let wld = ctx.ctx_rdf in
  let f uri =
    [ a_filetype ctx uri ;
      Grdf_ftype.extension wld uri ;
      Grdf_ftype.desc wld uri ;
    ]
  in
  let rows = List.map f ftypes in
  let contents = table ~heads rows in
  ([ctype ()], filetype_page ctx ~title: "Filetypes" contents)
;;

let get_version ctx uri =
  let wtitle = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api uri in
  let name = Grdf_version.name ctx.ctx_rdf uri in
  let title = a ~href: uri name in
  let contents = xhtml_of_intfs_of ctx uri in
  let navpath = xhtml_navpath ctx (Version uri) in
  ([ctype ()], tool_page ctx ~title ~wtitle ~navpath contents)
;;

let get_versions ctx tool =
  let contents = xhtml_of_versions_of ctx tool in
  let title = Printf.sprintf "Versions of %s" (Grdf_tool.name ctx.ctx_rdf tool) in
  let navpath = xhtml_navpath ctx (Versions tool) in
  ([ctype ()], tool_page ctx ~title ~navpath contents)
;;

let get_branch ctx uri =
  let name = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api uri in
  let dot = Grdf_dot.dot_of_branch ctx.ctx_rdf uri in
  let svg = dot_to_svg dot in
  let branches = xhtml_of_branches_of ctx uri in
  let intfs = xhtml_of_intfs_of ctx uri in
  let versions = xhtml_of_versions_of ctx uri in
  let tmpl = Rest_xpage.tmpl_file ctx.ctx_cfg "branch.tmpl" in
  let env = List.fold_left
    (fun e (name, v) -> Xtmpl.env_add_att name v e)
    Xtmpl.env_empty
    [
      "graph", svg ;
      "branches", branches ;
      "interfaces", intfs ;
      "versions", versions ;
    ]
  in
  let env = Xtmpl.env_of_list ~env (Rest_xpage.default_commands ctx.ctx_cfg) in
  let contents = Xtmpl.apply_from_file env tmpl in
  let navpath = xhtml_navpath ctx (Branch uri) in
  ([ctype ()], tool_page ctx ~title: name ~navpath contents)
;;

let get_branches ctx uri =
  let pre = "Branches of " in
  let name = Grdfs.remove_prefix ctx.ctx_cfg.Config.rest_api uri in
  let wtitle = Printf.sprintf "%s %s" pre name in
  let title = Printf.sprintf "%s %s" pre (a ~href: uri name) in
  let contents = xhtml_of_branches_of ctx uri in
  let navpath = xhtml_navpath ctx (Branches uri) in
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
  let rows = List.map (fun m -> [a_chain_module ctx m]) modules in
  let heads = ["Chain module"] in
  let (deps, error) =
    let config = ctx.ctx_cfg in
    let (cmods, errors) = Chn_io.load_chain_files config in
    let deps = Chn_ast.compute_deps ctx.ctx_rdf config cmods in
    let dot = Chn_ast.Dot_deps.dot_of_deps config.Config.rest_api deps in
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
    Printf.sprintf
      "<section>%s</section><section title=\"Dependencies\">%s</section>"
      (table ~heads rows)
      deps
  in
  let title = "Modules" in
  ([ctype ()], chain_page ctx ~title ~error contents)
;;

let get_chain_module ctx modname =
  let config = ctx.ctx_cfg in
  let file = Chn_io.file_of_modname config modname in
  let title = Printf.sprintf "Module %s"
    (Chn_types.string_of_chain_modname modname)
  in
  let cmod = Chn_io.chn_module_of_file file in
  let heads = ["Chain"] in
  let rows = List.map
    (fun chain ->
       let name = Chn_types.mk_chain_name modname
         chain.Chn_ast.chn_name
       in
         [a_chain ctx name]
    ) cmod.Chn_ast.cmod_chains
  in
  let deps =
    let deps = Chn_ast.compute_deps ctx.ctx_rdf config [cmod] in
    let dot =
      Chn_ast.Dot_deps.dot_of_deps config.Config.rest_api
      ~fullnames: false deps
    in
    dot_to_svg dot
  in
  let contents =
    Printf.sprintf
    "<section>%s</section><section title=\"Dependencies\">%s</section>"
    (table ~heads rows)
    deps
  in
  let navpath = xhtml_navpath ctx (Chain_module modname) in
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
      None -> ("No code found", "")
    | Some chn ->
        let prefix = config.Config.rest_api in
        let code = Rest_xpage.xhtml_of_chain prefix chn in
        let svg =
          let dot = Chn_ast.Dot.dot_of_chain ~prefix chn in
          dot_to_svg dot
        in
        (code, svg)
  in
  let navpath = xhtml_navpath ctx (Chain fullname) in
  let contents =
    Printf.sprintf "<section>%s</section><section title=\"Source from %s\">%s</section>"
      svg (Filename.quote file) code
  in
  ([ctype ()], chain_page ctx ~title ~navpath contents)
;;

let handle_chain_error f ctx p =
  try f ctx p
  with  e ->
      let title = "Error" in
      let error =
        match e with
          Loc.Problem pb -> Loc.string_of_problem pb
        | Sys_error msg | Failure msg -> msg
        | e -> Printexc.to_string e
      in
      let error = Printf.sprintf "<pre><![CDATA[%s]]></pre>" error in
      ([ctype ()], chain_page ctx ~title ~error "")
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
  | Chain_module modname -> handle_chain_error get_chain_module ctx modname
  | Chain fullname -> handle_chain_error get_chain ctx fullname

(*  | _ -> ([ctype ()], page ctx ~title: "Not implemented" "This page is not implemented yet")*)
;;
