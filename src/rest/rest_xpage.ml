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

(** Building xhtml pages. *)

module Iriset = Rdf_iri.Iriset

let star ?label () =
  let args = match label with
      None -> []
    | Some s -> [("","title"), [Xtmpl.D s]]
  in
  Xtmpl.E (("","star"), Xtmpl.atts_of_list args, []);;

let fun_star acc _ args _ =
  let args = Xtmpl.atts_of_list ~atts: args
    [ ("","width"), [Xtmpl.D "16"] ;
      ("", "src"), [Xtmpl.D "<site-url/>star.svg"] ;
    ]
  in
  (acc, [Xtmpl.E (("","img"), args, [])])
;;

let fun_include tmpl_dir acc _env args subs =
  match Xtmpl.get_arg_cdata args ("", "file") with
    None -> failwith "Missing 'file' argument for include command";
  | Some file ->
      let file =
        if Filename.is_relative file then
          Filename.concat tmpl_dir file
        else
          file
      in
      let xml = [Xtmpl.xml_of_string (Misc.string_of_file file)] in
      let args = Xtmpl.atts_one ~atts: args
        ("", "include-contents") subs
      in
      (acc, [Xtmpl.E (("", Xtmpl.tag_env), args, xml)])
;;
let fun_image acc _env args legend =
  let width = Xtmpl.opt_arg_cdata args ("", "width") in
  let src = Xtmpl.opt_arg_cdata args ("", "src") in
  let cls = Printf.sprintf "img%s"
    (match Xtmpl.get_arg_cdata args ("", "float") with
       Some "left" -> "-float-left"
     | Some "right" -> "-float-right"
     | Some s -> failwith (Printf.sprintf "unhandled image position: %s" s)
     | None -> ""
    )
  in
  let xmls =
    [
      Xtmpl.E (("", "div"), Xtmpl.atts_one ("", "class") [ Xtmpl.D cls ] ,
       (Xtmpl.E (("", "img"),
         Xtmpl.atts_of_list
           [ ("", "class"), [Xtmpl.D "img"] ;
             ("", "src"), [Xtmpl.D src] ;
             ("", "width"), [Xtmpl.D width] ;
           ],
           [])
       ) ::
         (match legend with
            [] -> []
          | xml -> [ Xtmpl.E (("", "div"), Xtmpl.atts_one ("", "class") [Xtmpl.D "legend"], xml) ]
         )
      )
    ]
  in
  (acc, xmls)
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

let fun_hcode ?(inline=false) ?lang acc _env args code =
  let language, language_options =
    match lang with
      None ->
        (
         let lang = Xtmpl.opt_arg_cdata args ~def: "txt" ("", "lang") in
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
  let xmls =
    if inline then
      [ Xtmpl.E (("", "span"), Xtmpl.atts_one ("", "class") [Xtmpl.D "icode"], [xml_code]) ]
    else
      [ Xtmpl.E (("", "pre"),
         Xtmpl.atts_one ("", "class") [Xtmpl.D ("code-%s"^language)],
         [xml_code])
      ]
  in
  (acc, xmls)
;;

let fun_ocaml = fun_hcode ~lang: "ocaml";;
let fun_command_line = fun_hcode ~lang: "sh";;
let fun_icode = fun_hcode ~inline: true ;;

let fun_section cls acc _env args body =
  let atts =
    match Xtmpl.get_arg_cdata args ("", "name") with
      None -> Xtmpl.atts_empty
    | Some name -> Xtmpl.atts_one ("", "id") [Xtmpl.D name]
  in
  let title =
    match Xtmpl.get_arg args ("", "title") with
      None -> []
    | Some t ->
        [Xtmpl.E (("", "div"),
           Xtmpl.atts_one ~atts ("", "class") [Xtmpl.D (cls^"-title")],
           t
          )]
  in
  (acc, [ Xtmpl.E (("", "div"),
      Xtmpl.atts_one ("", "class") [Xtmpl.D cls],
      title @ body) ])
;;

let fun_subsection = fun_section "subsection";;
let fun_section = fun_section "section";;

let fun_if acc env args subs =
  (*prerr_endline (Printf.sprintf "if: env=%s" (Xtmpl.string_of_env env));*)
  let pred (_,att) v =
    let (_, xmls) = Xtmpl.apply_to_string () env (Printf.sprintf "<%s/>" att) in
    let s = Xtmpl.string_of_xmls xmls in
    let sv = Xtmpl.string_of_xmls v in
    (*prerr_endline (Printf.sprintf "fun_if: pred: att=\"%s\", s=\"%s\", v=\"%s\"" att s v);*)
    s = sv
  in
  let cond = Xtmpl.Name_map.for_all pred args in
  let subs = List.filter
    (function Xtmpl.D _ -> false | _ -> true)
    subs
  in
  (*prerr_endline (Printf.sprintf "if: length(subs)=%d" (List.length subs));*)
  let xmls =
    match cond, subs with
    | true, [] -> failwith "<if>: missing children"
    | true, h :: _
    | false, _ :: h :: _ -> [h]
    | false, []
    | false, [_] -> []
  in
  (acc, xmls)
;;

let fun_site_url config acc _ _ _ =
  (acc, [ Xtmpl.D (Rdf_uri.string (Rdf_iri.to_uri config.Config.rest_api)) ]);;
let fun_site_title config acc _ _ _ = (acc, [ Xtmpl.D config.Config.project_name ]);;

let tmpl_dir config =
  List.fold_left Fname.concat_s config.Config.root_dir
  ["in" ; "web" ; "tmpl"]
;;
let tmpl_file config file = Fname.concat_s (tmpl_dir config) file;;

let default_commands config =

  [
    ("", "if"), fun_if ;
    ("", "include"), fun_include (Fname.abs_string (tmpl_dir config));
    ("", "image"), fun_image ;
    ("", "hcode"), fun_hcode ~inline: false ?lang: None;
    ("", "icode"), fun_icode ?lang: None;
    ("", "ocaml"), fun_ocaml ~inline: false ;
    ("", "star"), fun_star ;
    ("", "command-line"), fun_command_line ~inline: false ;
    ("", "section"), fun_section ;
    ("", "subsection"), fun_subsection ;
    ("", "site-url"), fun_site_url config;
    ("", "site-title"), fun_site_title config ;
    ]
;;

let page config ?env ~title ?javascript ?(wtitle=title) ?(navpath=[]) ?(error="") contents =
  let morehead =
    let code =
      match javascript with
        None -> "function onPageLoad() { }"
      | Some code -> code
    in
    [ Xtmpl.E (("", "script"), 
       Xtmpl.atts_one ("", "type") [Xtmpl.D "text/javascript"],
       [Xtmpl.D code]) 
    ]
  in
  let env = Xtmpl.env_of_list ?env
    ((("", "page-title"), (fun acc _ _ _ -> (acc, [Xtmpl.xml_of_string title]))) ::
     (("", "window-title"), (fun acc _ _ _ -> (acc, [Xtmpl.D wtitle]))) ::
     (("", "navpath"), (fun acc _ _ _ -> (acc, navpath))) ::
     (("", "error"), (fun acc _ _ _ -> (acc, [Xtmpl.xml_of_string error]))) ::
     (("", "morehead"), (fun acc _ _ _ -> (acc, morehead))) ::
     (default_commands config))
  in
  let f () env args body = ((), contents) in
  let env = Xtmpl.env_of_list ~env [("", "contents"), f] in
  let tmpl_file = tmpl_file config "page.tmpl" in
  snd (Xtmpl.apply_to_file () env (Fname.abs_string tmpl_file))
;;

open Chn_ast;;

class xhtml_ast_printer prefix =
  object(self)
    inherit Chn_ast.ast_printer

    method string_of_port p =
      let link ftype =
        let href = Rdf_iri.to_uri (Grdfs.iri_filetype ~prefix ftype) in
        Xtmpl.string_of_xml
        (Xtmpl.E (("", "a"),
            Xtmpl.atts_one ("", "href") [Xtmpl.D (Rdf_uri.string href)],
            [Xtmpl.D ftype]))
      in
      let ft = Grdf_port.string_of_port_type link p.p_ftype in
      Printf.sprintf "%s %s" ft p.p_name

    method string_of_op_origin = function
      Chain fullname ->
        Xtmpl.string_of_xml
        (Xtmpl.E
         (("", "a"), 
            Xtmpl.atts_one ("", "href")
              [Xtmpl.D (Rdf_uri.string (Rdf_iri.to_uri (Chn_types.iri_chain prefix fullname))) ],
            [Xtmpl.D (Chn_types.string_of_chain_name fullname)]))
    | Foreach (origin, port_ref) ->
        Printf.sprintf "foreach(%s, %s)"
        (self#string_of_op_origin origin) (self#string_of_port_ref port_ref)
    | Special _ -> assert false
    | Interface s ->
        try
          let href = Rdf_uri.string
            (Rdf_iri.to_uri (Chn_types.iri_intf_of_interface_spec ~prefix s))
          in
          Xtmpl.string_of_xml
          (Xtmpl.E (("", "a"),
              Xtmpl.atts_one ("", "href") [Xtmpl.D href],
              [Xtmpl.D (Printf.sprintf "%S" s)]))
        with
          Failure s ->
            Xtmpl.string_of_xml (Xtmpl.E (("", "i"), Xtmpl.atts_empty, [Xtmpl.D s]))

    method private kwd ?(cls="kwa")s =
      let cls = Printf.sprintf "hl %s" cls in
      Xtmpl.string_of_xml (Xtmpl.E (("", "span"),
        Xtmpl.atts_one ("", "class") [Xtmpl.D cls],
        [Xtmpl.D s])
      )

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
  [Xtmpl.xml_of_string (printer#string_of_chain chain)]
;;

class xhtml_chain_dot_printer prefix =
  object(self)
    inherit Chn_ast.chain_dot_printer as super

    method private get_port_type_iri t =
      Grdf_port.port_file_type_iri prefix t

    method print_port b color p =
      let link = self#get_port_type_iri p.p_ftype in
      let ft = Grdf_port.string_of_port_type (fun x -> x) p.p_ftype in
      Printf.bprintf b "%s [color=\"black\" fillcolor=\"%s\" style=\"filled\" \
                            shape=\"box\" href=\"%s\" label=\"%s:%s\"];\n"
        p.p_name color
        (match link with None -> "" | Some iri -> Rdf_uri.string (Rdf_iri.to_uri iri))
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
  let fchain = Chn_types.iri_fchain
    ctx.Chn_types.ctx_cfg.Config.rest_api fchain_name
  in
  let o = new xhtml_fchain_dot_printer in
  o#dot_of_fchain ctx fchain
;;


class xhtml_ichain_dot_printer =
  let dotp = new Chn_ast.chain_dot_printer in
  let get_origin ctx iri =
    match Grdfs.object_iri ctx.Chn_types.ctx_rdf
      ~sub: (Rdf_term.Iri iri) ~pred: Grdfs.genet_portfrom
    with
      None ->
        failwith
        (Printf.sprintf "get_origin %S => None" (Rdf_iri.string iri));
    | Some iri2 ->
        (*prerr_endline (Printf.sprintf "get_origin %S => %S"
         (Rdf_uri.string uri) (Rdf_uri.string uri2));*)
        iri2
  in
  object(self)
    method id s = "n"^(Digest.to_hex (Digest.string s))
    method iri_id iri = self#id (Rdf_iri.string iri)

    method color_of_port_dir = function
    | Grdf_port.In -> dotp#color_in
    | Grdf_port.Out -> dotp#color_out

    method port_link_and_name ctx iri =
      let ptype = Grdf_port.port_type ctx.Chn_types.ctx_rdf (get_origin ctx iri) in
      let name = Grdf_port.string_of_port_type (fun x -> x) ptype in
      let link = Chn_inst.instport_file_iri ctx iri in
      (link, name)

    method print_port_edges ctx b iri =
      prerr_endline (Printf.sprintf "Printing edges of %s" (Rdf_iri.string iri));
      let ports = Chn_flat.port_consumers ctx iri in
      let src = self#iri_id iri in
      let f p =
        Printf.bprintf b "%s -> %s ;\n" src (self#iri_id p)
      in
      List.iter f ports

    method print_port ctx b iri =
      let dir = Grdf_port.port_dir iri in
      let id = self#iri_id iri in
      let (link, name) = self#port_link_and_name ctx iri in
      let label =
        match Grdf_port.port_name ctx.Chn_types.ctx_rdf iri with
          "" -> string_of_int (Grdf_port.port_rank iri)
        | s -> s
      in
      Printf.bprintf b "%s [color=\"black\" fillcolor=\"%s\" \
                            style=\"filled\" shape=\"box\" \
                            href=\"%s\" label=\"%s:%s\" rank=%S];\n"
        id (self#color_of_port_dir dir)
        (match link with None -> "" | Some iri -> Rdf_uri.string (Rdf_iri.to_uri iri))
        label name
        (match dir with Grdf_port.In -> "min" | Grdf_port.Out -> "max")

    method print_op ctx ichain b acc_ports iri =
      let (color, label) =
        let iri_from = Chn_flat.get_op_origin ctx iri in
        match Grdf_intf.intf_exists ctx.Chn_types.ctx_rdf iri_from with
          None -> dotp#color_chain, Chn_flat.get_op_name iri
        | Some name ->
            let tool = Grdf_intf.tool_of_intf iri_from in
            let name = Printf.sprintf "%s / %s" (Grdf_tool.name ctx.Chn_types.ctx_rdf tool) name in
            dotp#color_interface, name
      in
      let color =
        if Chn_run.return_code ctx iri <> 0 then
          "red"
        else
          color
      in
      if not (Rdf_iri.equal ichain iri) then
        Printf.bprintf b "subgraph cluster_%s {\n\
             label=%S;\n color=\"black\" fillcolor=%S;\n\
             style=\"filled\"; href=%S;\n"
        (self#iri_id iri) label color
        (Rdf_uri.string (Rdf_iri.to_uri iri));
      let in_ports = Grdf_port.ports ctx.Chn_types.ctx_rdf iri Grdf_port.In in
      let out_ports = Grdf_port.ports ctx.Chn_types.ctx_rdf iri Grdf_port.Out in
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
               (self#iri_id in_p) (self#iri_id out_p)
            )
            out_ports
      end;
      let ports =
        if not (Rdf_iri.equal ichain iri) then
          ( Buffer.add_string b "}\n"; out_ports )
        else
          in_ports
      in
      List.fold_right Iriset.add ports acc_ports

    method dot_of_chain ctx iri =
      let b = Buffer.create 256 in
      Buffer.add_string b "digraph g {\nrankdir=LR;\nfontsize=10;\n";
      let ports = List.fold_left
        (self#print_op ctx iri b) Iriset.empty (iri :: (Chn_flat.get_ops ctx iri))
      in
      Iriset.iter (self#print_port_edges ctx b) ports;
      Buffer.add_string b "}\n";
      Buffer.contents b
  end

let dot_of_ichain_op ctx uri =
  let o = new xhtml_ichain_dot_printer in
  let dot = o#dot_of_chain ctx uri in
  Misc.file_of_string ~file: "/tmp/instgraph.dot" dot;
  dot
;;

let dot_of_ichain ctx ichain_name =
  let iri = Chn_types.iri_ichain
    ctx.Chn_types.ctx_cfg.Config.rest_api ichain_name
  in
  let o = new xhtml_ichain_dot_printer in
  let dot = o#dot_of_chain ctx iri in
  Misc.file_of_string ~file: "/tmp/instgraph.dot" dot;
  dot
;;

