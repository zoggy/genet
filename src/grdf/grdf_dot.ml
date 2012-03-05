(** *)

let md5 s = Digest.to_hex (Digest.string s);;

let dot_id uri = Printf.sprintf "N%s" (md5 uri);;

let gen_tool b wld uri =
  let name = Grdf_tool.name wld uri in
  Printf.bprintf b "%s [ label=\"%s\", shape=\"box\", href=\"%s\"];\n"
    (dot_id uri) (String.escaped name) uri
;;

let gen_version b wld uri =
  let name = Grdf_version.name wld uri in
  Printf.bprintf b
  "%s [ label=\"%s\", fillcolor=\"palegreen\", style=\"filled\", shape=\"box3d\", href=\"%s\"];\n"
  (dot_id uri) (String.escaped name) uri
;;

let gen_branch b wld t =
  let uri = t.Grdf_branch.bch_uri in
  let name = Grdf_version.name wld uri in
  Printf.bprintf b
  "%s [ label=\"%s\", fillcolor=\"white\", style=\"\", shape=\"ellipse\", href=\"%s\"];\n"
  (dot_id uri) (String.escaped name) uri
;;

let gen_intf b wld uri =
  let name = Grdf_intf.name wld uri in
  Printf.bprintf b
  "%s [ label=\"%s\", fillcolor=\"olivedrab1\", style=\"filled\", shape=\"octagon\", href=\"%s\"];\n"
  (dot_id uri) (String.escaped name) uri
;;

let gen_hasbranch b wld uri =
  let l = Grdf_branch.subs wld uri in
  List.iter
    (fun uri2 -> Printf.bprintf b "%s -> %s [label=\"hasBranch\"];\n"
     (dot_id uri) (dot_id uri2))
     l
;;

let gen_hasversion b wld uri =
  let l = Grdf_version.versions_of wld uri in
  List.iter
    (fun uri2 -> Printf.bprintf b "%s -> %s [label=\"hasVersion\"];\n"
     (dot_id uri) (dot_id uri2))
     l
;;

let gen_hasintf b wld uri =
  let l = Grdf_intf.intfs_of wld uri in
  List.iter
    (fun uri2 -> Printf.bprintf b "%s -> %s [label=\"hasIntf\"];\n"
     (dot_id uri) (dot_id uri2))
     l
;;

let dot wld =
  let tools = Grdf_tool.tools wld in
  let versions = Grdf_version.versions wld in
  let branches = Grdf_branch.branches wld in
  let intfs = Grdf_intf.intfs wld in
  let b = Buffer.create 256 in
  Buffer.add_string b "digraph g {\nrankdir=TB;\n";
  List.iter (gen_tool b wld) tools;
  List.iter (gen_version b wld) versions;
  List.iter (gen_branch b wld) branches;
  List.iter (gen_intf b wld) intfs;
  List.iter (gen_hasbranch b wld) (tools @ (List.map (fun t -> t.Grdf_branch.bch_uri) branches));
  List.iter (gen_hasversion b wld) (tools @ (List.map (fun t -> t.Grdf_branch.bch_uri) branches));
  List.iter (gen_hasintf b wld) (tools @ (List.map (fun t -> t.Grdf_branch.bch_uri) branches));

  Buffer.add_string b "}\n";
  Buffer.contents b
;;

let dot_to_svg dot =
  let temp_file = Filename.temp_file "genet" "svg" in
  let com = Printf.sprintf "echo %s | dot -Tsvg | tail --lines=+7 > %s"
    (Filename.quote dot) (Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let svg = Misc.string_of_file temp_file in
      Sys.remove temp_file;
      svg
  | n ->
      let msg = Printf.sprintf "Execution failed (%d): %s" n com in
      failwith msg
;;



  