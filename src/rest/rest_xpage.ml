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
  let pred (att, v) =
    let s = Xtmpl.apply env (Printf.sprintf "<%s/>" att) in
    (*prerr_endline (Printf.sprintf "fun_if: pred: att=%s, s=%s, v=%s" att s v);*)
    s = v
  in
  let cond = List.for_all pred args in
  match cond, subs with
  | true, [] -> failwith "<if>: missing children"
  | true, h :: _
  | false, _ :: h :: _ -> [h]
  | false, []
  | false, [_] -> []
;;

let fun_site_url config _ _ _ = [ Xtmpl.D config.Config.rest_api ];;
let fun_site_title config _ _ _ = [ Xtmpl.D config.Config.project_name ];;

let tmpl_dir config =
  List.fold_left Filename.concat config.Config.root_dir
  ["in" ; "web" ; "tmpl"]
;;

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

let page config ?env ~title contents =
  let env = Xtmpl.env_of_list ?env
    (("page-title", (fun _ _ _ -> [Xtmpl.D title])) :: (default_commands config))
  in
  let f env args body = contents in
  let env = Xtmpl.env_of_list ~env ["contents", f] in
  let tmpl_file = Filename.concat (tmpl_dir config) "page.tmpl" in
  Xtmpl .apply_from_file env tmpl_file
;;
