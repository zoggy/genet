(** Building xhtml pages. *)

let fun_include tmpl_dir _env args subs =
  match Xtmpl.get_arg args "file" with
    None -> failwith "Missing 'file' argument for include command";
  | Some file ->
      let file =
        if Filename.is_relative file then
          Filename.concat tmpl_dir file
        else
          file
      in
      let xml = [Xtmpl.xml_of_string (Misc.string_of_file file)] in
      let args =
        ("include-contents", String.concat "" (List.map Xtmpl.string_of_xml subs)) ::
        args
      in
      [Xtmpl.T (Xtmpl.tag_env, args, xml)]
;;
let fun_image _env args legend =
  let width = Xtmpl.opt_arg args "width" in
  let src = Xtmpl.opt_arg args "src" in
  let cls = Printf.sprintf "img%s"
    (match Xtmpl.get_arg args "float" with
       Some "left" -> "-float-left"
     | Some "right" -> "-float-right"
     | Some s -> failwith (Printf.sprintf "unhandled image position: %s" s)
     | None -> ""
    )
  in
  [
    Xtmpl.T ("div", [ "class", cls ],
     (Xtmpl.T ("img", [ "class", "img" ; "src", src; "width", width ], [])) ::
     (match legend with
        [] -> []
      | xml -> [ Xtmpl.T ("div", ["class", "legend"], xml) ]
     )
    )
  ]
;;

let highlight ~opts code =
  let code_file = Filename.temp_file "stog" "code" in
  Misc.file_of_string ~file: code_file code;
  let temp_file = Filename.temp_file "stog" "highlight" in
  let com = Printf.sprintf
    "highlight -O xhtml %s -f %s > %s"
    opts (Filename.quote code_file)(Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let code = Misc.string_of_file temp_file in
      Sys.remove code_file;
      Sys.remove temp_file;
      code
  | _ ->
      failwith (Printf.sprintf "command failed: %s" com)
;;

let fun_hcode ?(inline=false) ?lang _env args code =
  let language, language_options =
    match lang with
      None ->
        (
         let lang = Xtmpl.opt_arg args ~def: "txt" "lang" in
         match lang with
           "txt" -> (lang, None)
         | _ -> (lang, Some (Printf.sprintf "--syntax=%s" lang))
        )
    | Some "ocaml" ->
        ("ocaml", Some (Printf.sprintf "--config-file=%s/ocaml.lang" (Filename.dirname Sys.argv.(0))))
    | Some lang ->
        (lang, Some (Printf.sprintf "--syntax=%s" lang))
  in
  let code =
    match code with
      [ Xtmpl.D code ] -> code
    | [] -> ""
    | _ -> failwith (Printf.sprintf "Invalid code: %s"
         (String.concat "" (List.map Xtmpl.string_of_xml code)))
  in
  let code = Misc.strip_string code in
  let xml_code =
    match language_options with
      None -> Xtmpl.D code
    | Some opts ->
        let code = highlight ~opts code in
        Xtmpl.xml_of_string code
  in
  if inline then
    [ Xtmpl.T ("span", ["class","icode"], [xml_code]) ]
  else
    [ Xtmpl.T ("pre",
       ["class", Printf.sprintf "code-%s" language], [xml_code])
    ]
;;

let fun_ocaml = fun_hcode ~lang: "ocaml";;
let fun_command_line = fun_hcode ~lang: "sh";;
let fun_icode = fun_hcode ~inline: true ;;

let fun_section cls _env args body =
  let id =
    match Xtmpl.get_arg args "name" with
      None -> []
    | Some name -> ["id", name]
  in
  let title =
    match Xtmpl.get_arg args "title" with
      None -> []
    | Some t ->
        [Xtmpl.T ("div", ["class", cls^"-title"] @ id, [Xtmpl.xml_of_string t])]
  in
  [ Xtmpl.T ("div", ["class", cls], title @ body) ]
;;

let fun_subsection = fun_section "subsection";;
let fun_section = fun_section "section";;

let fun_if env args subs =
  prerr_endline (Printf.sprintf "if: env=%s" (Xtmpl.string_of_env env));
  let pred (att, v) =
    let s = Xtmpl.apply env (Printf.sprintf "<%s/>" att) in
    (*prerr_endline (Printf.sprintf "fun_if: pred: att=\"%s\", s=\"%s\", v=\"%s\"" att s v);*)
    s = v
  in
  let cond = List.for_all pred args in
  let subs = List.filter
    (function Xtmpl.D _ -> false | _ -> true)
    subs
  in
  (*prerr_endline (Printf.sprintf "if: length(subs)=%d" (List.length subs));*)
  match cond, subs with
  | true, [] -> failwith "<if>: missing children"
  | true, h :: _
  | false, _ :: h :: _ -> [h]
  | false, []
  | false, [_] -> []
;;

let fun_site_url config _ _ _ = [ Xtmpl.D (Rdf_uri.string config.Config.rest_api) ];;
let fun_site_title config _ _ _ = [ Xtmpl.D config.Config.project_name ];;

let tmpl_dir config =
  List.fold_left Filename.concat config.Config.root_dir
  ["in" ; "web" ; "tmpl"]
;;
let tmpl_file config file = Filename.concat (tmpl_dir config) file;;

let default_commands config =

  [
    "if", fun_if ;
    "include", fun_include (tmpl_dir config);
    "image", fun_image ;
    "hcode", fun_hcode ~inline: false ?lang: None;
    "icode", fun_icode ?lang: None;
    "ocaml", fun_ocaml ~inline: false ;
    "command-line", fun_command_line ~inline: false ;
    "section", fun_section ;
    "subsection", fun_subsection ;
    "site-url", fun_site_url config;
    "site-title", fun_site_title config ;
    ]
;;

let page config ?env ~title ?javascript ?(wtitle=title) ?(navpath="") ?(error="") contents =
  let morehead =
    let code =
      match javascript with
        None -> "function onPageLoad() { }"
      | Some code -> code
    in
    [ Xtmpl.T ("script", ["type", "text/javascript"], [Xtmpl.D code]) ]
  in
  let env = Xtmpl.env_of_list ?env
    (("page-title", (fun _ _ _ -> [Xtmpl.xml_of_string title])) ::
     ("window-title", (fun _ _ _ -> [Xtmpl.D wtitle])) ::
     ("navpath", (fun _ _ _ -> [Xtmpl.xml_of_string navpath])) ::
     ("error", (fun _ _  _ -> [Xtmpl.xml_of_string error])) ::
     ("morehead", (fun _ _ _ -> morehead)) ::
     (default_commands config))
  in
  let f env args body = contents in
  let env = Xtmpl.env_of_list ~env ["contents", f] in
  let tmpl_file = tmpl_file config "page.tmpl" in
  Xtmpl .apply_from_file env tmpl_file
;;

open Chn_ast;;

class xhtml_ast_printer prefix =
  object(self)
    inherit Chn_ast.ast_printer

    method string_of_port p =
      let link ftype =
        let href = Grdfs.uri_filetype ~prefix ftype in
        Xtmpl.string_of_xml
        (Xtmpl.T ("a", ["href", Rdf_uri.string href], [Xtmpl.D ftype]))
      in
      let ft = Grdf_port.string_of_port_type link p.p_ftype in
      Printf.sprintf "%s %s" ft p.p_name

    method string_of_op_origin = function
      Chain fullname ->
        Xtmpl.string_of_xml
        (Xtmpl.T
         ("a", ["href", Rdf_uri.string (Chn_types.uri_chain prefix fullname)],
          [Xtmpl.D (Chn_types.string_of_chain_name fullname)]))
    | Foreach (origin, port_ref) ->
        Printf.sprintf "foreach(%s, %s)"
        (self#string_of_op_origin origin) (self#string_of_port_ref port_ref)
    | Special _ -> assert false
    | Interface s ->
        try
          let href = Rdf_uri.string
            (Chn_types.uri_intf_of_interface_spec ~prefix s)
          in
          Xtmpl.string_of_xml
          (Xtmpl.T
           ("a", ["href", href], [Xtmpl.D (Printf.sprintf "%S" s)]))
        with
          Failure s ->
            Xtmpl.string_of_xml (Xtmpl.T ("i", [], [Xtmpl.D s]))

    method private kwd ?(cls="kwa")s =
      let cls = Printf.sprintf "hl %s" cls in
      Xtmpl.string_of_xml (Xtmpl.T ("span",["class", cls], [Xtmpl.D s]))

    method string_of_operation op =
      Printf.sprintf "  %s %s : %s ;\n"
      (self#kwd ~cls: "kwb" "operation")
      op.op_name (self#string_of_op_origin op.op_from)

    method private string_of_chain_in_pre chn =
      let b = Buffer.create 256 in
      Printf.bprintf b "%s %s \n%s\n{\n"
      (self#kwd "chain")
      (Chn_types.string_of_chain_basename chn.chn_name)
      (self#kwd ~cls:"com" (Printf.sprintf "(* %s *)" chn.chn_comment)) ;

      Printf.bprintf b "  %s: %s ;\n"
      (self#kwd ~cls: "kwb" "in") (self#string_of_port_array chn.chn_inputs) ;

      Printf.bprintf b "  %s: %s ;\n"
      (self#kwd ~cls: "kwb" "out") (self#string_of_port_array chn.chn_outputs) ;

      Printf.bprintf b "\n%s" (self#string_of_operation_list chn.chn_ops);

      Printf.bprintf b "\n%s" (self#string_of_edge_list chn.chn_edges);
      Buffer.add_string b "}\n";
      Buffer.contents b

    method! string_of_chain chn =
     Printf.sprintf "<pre>%s</pre>" (self#string_of_chain_in_pre chn)

    method string_of_chn_module cmod =
      Printf.sprintf "<pre>%s</pre>"
      (String.concat "\n" (List.map self#string_of_chain_in_pre cmod.cmod_chains))
  end
;;

let xhtml_of_chain prefix chain =
  let printer = new xhtml_ast_printer prefix in
  printer#string_of_chain chain
;;

class xhtml_chain_dot_printer prefix =
  object(self)
    inherit Chn_ast.chain_dot_printer as super

    method private get_port_type_uri t =
      Grdf_port.port_file_type_uri prefix t

    method print_port b color p =
      let link = self#get_port_type_uri p.p_ftype in
      let ft = Grdf_port.string_of_port_type (fun x -> x) p.p_ftype in
      Printf.bprintf b "%s [color=\"black\" fillcolor=\"%s\" style=\"filled\" \
                            shape=\"box\" href=\"%s\" label=\"%s:%s\"];\n"
        p.p_name color
        (match link with None -> "" | Some uri -> Rdf_uri.string uri)
         p.p_name ft

  end;;

let dot_of_chain prefix chain =
  let o = new xhtml_chain_dot_printer prefix in
  o#dot_of_chain ~prefix chain
;;

class xhtml_chain_dot_deps prefix =
  let printer = new xhtml_chain_dot_printer prefix in
  object(self)
    inherit Chn_ast.chain_dot_deps ~chain_dot: printer ()as super
  end
;;

let dot_of_deps prefix ?fullnames deps =
  let printer = new xhtml_chain_dot_deps prefix in
  printer#dot_of_deps prefix ?fullnames deps
;;

class xhtml_fchain_dot_printer =
  object(self)
    inherit Chn_flat.fchain_dot_printer as super

  end;;

let dot_of_fchain ctx fchain_name =
  let fchain = Chn_types.uri_fchain
    ctx.Chn_types.ctx_cfg.Config.rest_api fchain_name
  in
  let o = new xhtml_fchain_dot_printer in
  o#dot_of_fchain ctx fchain
;;

class xhtml_ichain_dot_printer =
  let dotp = new Chn_ast.chain_dot_printer in
  let get_origin ctx uri =
    match Grdfs.object_uri ctx.Chn_types.ctx_rdf
      ~sub: (Rdf_node.Uri uri) ~pred: Grdfs.genet_opfrom
    with
      None ->
        failwith
        (Printf.sprintf "get_origin %S => None" (Rdf_uri.string uri));
    | Some uri2 ->
        (*prerr_endline (Printf.sprintf "get_origin %S => %S"
         (Rdf_uri.string uri) (Rdf_uri.string uri2));*)
        uri2
  in
  object(self)
    method id s = "n"^(Digest.to_hex (Digest.string s))
    method uri_id uri = self#id (Rdf_uri.string uri)

    method color_of_port_dir = function
    | Grdf_port.In -> dotp#color_in
    | Grdf_port.Out -> dotp#color_out

    method port_link_and_name ctx uri =
      let ptype = Grdf_port.port_type ctx.Chn_types.ctx_rdf (get_origin ctx uri) in
      let name = Grdf_port.string_of_port_type (fun x -> x) ptype in
      let link =
        match Grdfs.object_literal ctx.Chn_types.ctx_rdf
          ~sub: (Rdf_node.Uri uri) ~pred: Grdfs.genet_filemd5
        with
        | Some md5 ->
            Some (Grdfs.uri_outfile_path ctx.Chn_types.ctx_cfg.Config.rest_api [md5])
        | None ->
            match Chn_flat.port_producers ctx uri with
              [] -> None
            | p :: _ ->
                match
                  Grdfs.object_literal ctx.Chn_types.ctx_rdf
                  ~sub: (Rdf_node.Uri p) ~pred: Grdfs.genet_filemd5
                with
                | Some md5 ->
                    Some (Grdfs.uri_outfile_path ctx.Chn_types.ctx_cfg.Config.rest_api [md5])
                | None -> None
      in
      (link, name)

    method print_port_edges ctx b uri =
      prerr_endline (Printf.sprintf "Printing edges of %s" (Rdf_uri.string uri));
      let ports = Chn_flat.port_consumers ctx uri in
      let src = self#uri_id uri in
      let f p =
        Printf.bprintf b "%s -> %s ;\n" src (self#uri_id p)
      in
      List.iter f ports

    method print_port ctx b uri =
      let dir = Grdf_port.port_dir uri in
      let id = self#uri_id uri in
      let (link, name) = self#port_link_and_name ctx uri in
      let label =
        match Grdf_port.port_name ctx.Chn_types.ctx_rdf uri with
          "" -> string_of_int (Grdf_port.port_rank uri)
        | s -> s
      in
      Printf.bprintf b "%s [color=\"black\" fillcolor=\"%s\" \
                            style=\"filled\" shape=\"box\" \
                            href=\"%s\" label=\"%s:%s\" rank=%S];\n"
        id (self#color_of_port_dir dir)
        (match link with None -> "" | Some uri -> Rdf_uri.string uri)
        label name
        (match dir with Grdf_port.In -> "min" | Grdf_port.Out -> "max")

    method print_op ctx ichain b acc_ports uri =
      let (color, label, href) =
        let uri_from = Chn_flat.get_op_origin ctx uri in
        match Grdf_intf.intf_exists ctx.Chn_types.ctx_rdf uri_from with
          None -> dotp#color_chain, Chn_flat.get_op_name uri, uri
        | Some name ->
            let tool = Grdf_intf.tool_of_intf uri_from in
            let name = Printf.sprintf "%s / %s" (Grdf_tool.name ctx.Chn_types.ctx_rdf tool) name in
            dotp#color_interface, name, uri_from
      in
      if not (Rdf_uri.equal ichain uri) then
        Printf.bprintf b "subgraph cluster_%s {\n\
             label=%S;\n color=\"black\" fillcolor=%S;\n\
             style=\"filled\"; href=%S;\n"
        (self#uri_id uri) label color
        (Rdf_uri.string href);
      let in_ports = Grdf_port.ports ctx.Chn_types.ctx_rdf uri Grdf_port.In in
      let out_ports = Grdf_port.ports ctx.Chn_types.ctx_rdf uri Grdf_port.Out in
      List.iter (self#print_port ctx b) in_ports;
      List.iter (self#print_port ctx b) out_ports;
      (* align in ports and out ports on two column, using invisible edges *)
      begin
        match in_ports with
          [] -> ()
        | in_p :: _ ->
            List.iter
            (fun out_p ->
               Printf.bprintf b "%s -> %s [style=\"invis\"];\n"
               (self#uri_id in_p) (self#uri_id out_p)
            )
            out_ports
      end;
      let ports =
        if not (Rdf_uri.equal ichain uri) then
          ( Buffer.add_string b "}\n"; out_ports )
        else
          in_ports
      in
      List.fold_right Uriset.add ports acc_ports

    method dot_of_chain ctx uri =
      let b = Buffer.create 256 in
      Buffer.add_string b "digraph g {\nrankdir=LR;\nfontsize=10;\n";
      let ports = List.fold_left
        (self#print_op ctx uri b) Uriset.empty (uri :: (Chn_flat.get_ops ctx uri))
      in
      Uriset.iter (self#print_port_edges ctx b) ports;
      Buffer.add_string b "}\n";
      Buffer.contents b
  end

let dot_of_ichain ctx ichain_name =
  let uri = Chn_types.uri_ichain
    ctx.Chn_types.ctx_cfg.Config.rest_api ichain_name
  in
  let o = new xhtml_ichain_dot_printer in
  let dot = o#dot_of_chain ctx uri in
  Misc.file_of_string ~file: "/tmp/instgraph.dot" dot;
  dot
;;

