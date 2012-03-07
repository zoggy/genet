(** *)

let md5 s = Digest.to_hex (Digest.string s);;

let dot_id uri = Printf.sprintf "N%s" (md5 uri);;

let tool_bgcolor2 = "#d7d7d9";;
let tool_bgcolor = "#e7e7e9";;
let tool_fgcolor = "black";;

let version_bgcolor = "#a1d99b";;
let version_fgcolor = "black";;

let branch_bgcolor = "#ccebc5";;
let branch_fgcolor = "black";;

let intf_bgcolor = "#fdcdac";;
let intf_fgcolor = "black";;

let ftype_bgcolor = "#0868ac";;
let ftype_fgcolor = "#white";;


let gen_tool b wld uri =
  let name = Grdf_tool.name wld uri in
  Printf.bprintf b "%s [ label=\"%s\", style=\"filled\", fillcolor=\"%s\", fontcolor=\"%s\", shape=\"box\", href=\"%s\"];\n"
    (dot_id uri) (String.escaped name) tool_bgcolor tool_fgcolor uri
;;

let gen_version b wld uri =
  let name = Grdf_version.name wld uri in
  Printf.bprintf b
  "%s [ label=\"%s\", style=\"filled\", fillcolor=\"%s\", fontcolor=\"%s\", shape=\"box3d\", href=\"%s\"];\n"
  (dot_id uri) (String.escaped name) version_bgcolor version_fgcolor uri
;;

let gen_branch b wld uri =
  let name = Grdf_version.name wld uri in
  Printf.bprintf b
  "%s [ label=\"%s\", style=\"filled\", fillcolor=\"%s\", fontcolor=\"%s\", shape=\"ellipse\", href=\"%s\"];\n"
  (dot_id uri) (String.escaped name) branch_bgcolor branch_fgcolor uri
;;

let gen_intf b wld uri =
  let name = Grdf_intf.name wld uri in
  let intf =
    Printf.sprintf "%s : %s"
    name
    (Grdf_intf.string_of_intf wld ~with_uri: false uri)
  in
  Printf.bprintf b
  "%s [ label=\"%s\", style=\"filled\", fillcolor=\"%s\", fontcolor=\"%s\", shape=\"octagon\", tooltip=\"%s\", href=\"%s\"];\n"
  (dot_id uri) (String.escaped name) intf_bgcolor intf_fgcolor (String.escaped intf) uri
;;

let gen_hasbranch b wld ?(label=true) ?pred uri =
  let l = Grdf_branch.subs wld uri in
  let l = match pred with None -> l | Some f -> List.filter f l in
  List.iter
  (fun uri2 -> Printf.bprintf b "%s -> %s [%s];\n"
     (dot_id uri) (dot_id uri2)
     (if label then "label=\"hasBranch\"" else "")
  )
     l
;;

let gen_hasversion b wld ?(label=true) ?pred uri =
  let l = Grdf_version.versions_of wld uri in
  let l = match pred with None -> l | Some f -> List.filter f l in
  List.iter
    (fun uri2 -> Printf.bprintf b "%s -> %s [%s];\n"
     (dot_id uri) (dot_id uri2)
     (if label then "label=\"hasVersion\"" else "")
  )
     l
;;

let gen_hasintf b wld ?(label=true) ?pred uri =
  let l = Grdf_intf.intfs_of wld uri in
  let l = match pred with None -> l | Some f -> List.filter f l in
  List.iter
    (fun uri2 -> Printf.bprintf b "%s -> %s [%s];\n"
     (dot_id uri) (dot_id uri2)
     (if label then "label=\"hasIntf\"" else "")
  )
     l
;;

let dot ?(edge_labels=true) wld =
  let tools = Grdf_tool.tools wld in
  let versions = Grdf_version.versions wld in
  let branches = List.map (fun t -> t.Grdf_branch.bch_uri) (Grdf_branch.branches wld) in
  let intfs = Grdf_intf.intfs wld in
  let b = Buffer.create 256 in
  Buffer.add_string b "digraph g {colorscheme=\"brewer\";\nrankdir=TB;\n";
  List.iter (gen_tool b wld) tools;
  List.iter (gen_version b wld) versions;
  List.iter (gen_branch b wld) branches;
  List.iter (gen_intf b wld) intfs;

  let label = edge_labels in
  List.iter (gen_hasbranch b ~label wld) (tools @ branches);
  List.iter (gen_hasversion b ~label wld) (tools @ branches);
  List.iter (gen_hasintf b ~label wld) (tools @ branches);

  Buffer.add_string b "}\n";
  Buffer.contents b
;;

let dot_of_tool wld tool =
  let b = Buffer.create 256 in
  Buffer.add_string b "digraph g {\nrankdir=TB;\nfontsize=10;\n";
  let versions = Grdf_version.versions_of wld ~recur: true tool in
  let branches = Grdf_branch.subs wld ~recur:true tool in
  let intfs = Grdf_intf.intfs_of_tool wld tool in
  gen_tool b wld tool;
  List.iter (gen_version b wld) versions;
  List.iter (gen_branch b wld) branches;
  List.iter (gen_intf b wld) intfs;

  let all = tool :: branches in
  let pred = fun uri -> List.mem uri all in
  List.iter (gen_hasbranch b wld ~label: false ~pred) all;

  let all = all @ versions in
  let pred = fun uri -> List.mem uri all in
  List.iter (gen_hasversion b wld ~label: false ~pred) all;

  let all = all @ intfs in
  let pred = fun uri -> List.mem uri all in
  List.iter (gen_hasintf b wld ~label: false ~pred) all;

  Buffer.add_string b "}\n";
  Buffer.contents b
;;

let dot_to_svg ?(options="") ?size dot =
  let temp_file = Filename.temp_file "genet" "svg" in
  let com = Printf.sprintf "echo %s |dot %s -Tsvg | tail --lines=+%d > %s"
    (Filename.quote dot) options
    (match size with None -> 7 | Some _ -> 9)
    (Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let svg = Misc.string_of_file temp_file in
      Sys.remove temp_file;
      let svg =
        match size with
          None -> svg
        | Some (w,h) ->
          Printf.sprintf "<svg width=\"%d\" height=\"%d\" viewBox=\"0.0 0.0 %d.00 %d.00\"\n
          xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n%s"
            w h w h svg
      in
      svg
  | n ->
      let msg = Printf.sprintf "Execution failed (%d): %s" n com in
      failwith msg
;;



  