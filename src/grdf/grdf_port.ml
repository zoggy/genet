(** *)

open Rdf_node;;
open Grdf_types;;

let dbg = Misc.create_log_fun
  ~prefix: "Chn_flat"
    "GENET_GRDF_PORT_DEBUG_LEVEL"
;;
type 'a port_type = One of 'a | List of 'a;;
type dir = In | Out ;;

let pred_of_dir = function
| In -> Grdfs.genet_consumes
| Out -> Grdfs.genet_produces
;;

let port_name wld port = Grdfs.name wld (Uri port);;
let port_rank = Grdfs.port_rank ;;

let port_type wld port =
  match Grdfs.object_uri wld ~sub: (Uri port) ~pred: Grdfs.genet_hasfiletype with
    Some uri -> One uri
  | None ->
      match Grdfs.object_uri wld ~sub: (Uri port) ~pred: Grdfs.genet_hasfiletypelist with
        Some uri -> List uri
      | None -> failwith (Printf.sprintf "No type for port %s" (Rdf_uri.string port))
;;

let sort_ports l =
  List.sort (fun p1 p2 ->
    Pervasives.compare (port_rank p1) (port_rank p2))
  l
;;
let ports wld intf dir =
  let pred = pred_of_dir dir in
  sort_ports (Grdfs.object_uris wld ~sub: (Uri intf) ~pred)
;;


let unset_port_type wld port =
  let preds = [ Grdfs.genet_hasfiletype ; Grdfs.genet_hasfiletypelist ] in
  let sub = Uri port in
  let f pred =
    List.iter
    (fun obj -> Grdfs.rem_triple wld ~sub ~pred: (Uri pred) ~obj: (Uri obj))
      (Grdfs.object_uris wld ~sub ~pred)
  in
  List.iter f preds
;;

let set_port_type wld port typ =
  let (pred, ftype_uri) =
    match typ with
      One uri -> (Grdfs.genet_hasfiletype, uri)
    | List uri -> (Grdfs.genet_hasfiletypelist, uri)
  in
  unset_port_type wld port;
  Grdfs.add_triple wld
     ~sub: (Uri port)
     ~pred: (Uri pred)
     ~obj: (Uri ftype_uri)
;;

let uri_intf_port_of_dir = function
  In -> Grdfs.uri_intf_in_port
| Out -> Grdfs.uri_intf_out_port
;;

let insert_port wld intf dir (n, typ, name) =
  let pred = pred_of_dir dir in
  let uri = (uri_intf_port_of_dir dir) intf n in
  let sub = Uri intf in
  let pred = Uri pred in
  let obj = Uri uri in
  Grdfs.add_triple wld ~sub ~pred ~obj;
  set_port_type wld uri typ;
  match name with None -> () | Some name -> Grdfs.add_name wld sub name
;;

let delete_ports wld uri dir =
  let dir_pred = pred_of_dir dir in
  let sub = Uri uri in
  let f pred =
    let ports = Grdfs.object_uris wld ~sub ~pred: dir_pred in
    List.iter
      (fun obj -> Grdfs.rem_triple wld ~sub ~pred: (Uri dir_pred) ~obj: (Uri obj))
      ports;
    let f port =
      List.iter
        (fun obj -> Grdfs.rem_triple wld ~sub: (Uri port) ~pred: (Uri pred) ~obj: (Uri obj))
        (Grdfs.object_uris wld ~sub: (Uri port) ~pred: pred)
    in
    List.iter f ports
  in
  let preds = [ Grdfs.genet_hasfiletype ; Grdfs.genet_hasfiletypelist ] in
  List.iter f preds
;;
(*
let delete_ports wld dir uri =
  let pred = pred_of_dir dir in
  let query =
    let triples1 = [
        (`I uri, [ `I pred, [`V "seq"] ] );
        (`V "seq", [ `V "seq_index", [`V "uri"] ]) ;
      ]
    in
    let triples2 =
      [ (`V "uri", [ `I Grdfs.genet_listof, [ `V "uri2"]]) ]
    in
    { Rdf_sparql.delins_insert = None ;
      delins_delete = Some (Some (triples1 @ triples2), []) ;
      delins_where = (triples1, Some (`Optional (triples2, None))) ;
    }
  in
  let query = Rdf_sparql.Delete_insert query in
  ignore (Rdf_sparql.exec wld.wld_world wld.wld_model query)
;;
*)
let set_ports wld intf dir ports =
  delete_ports wld intf dir ;
  List.iter (insert_port wld intf dir) ports
;;

let copy_ports wld ~src ~dst =
  dbg ~level:2 ~loc:"copy_ports"
    (fun () ->
      Printf.sprintf "src=%s dst=%s"
        (Rdf_uri.string src) (Rdf_uri.string dst));
  let map uri =
    (port_rank uri, port_type wld uri, Some (port_name wld uri))
  in
  let f_dir dir =
    let ports = ports wld src dir in
    set_ports wld dst dir (List.map map ports)
  in
  f_dir In ;
  f_dir Out
;;

let add_port wld intf dir ?(pos=max_int) ?name filetype =
  let pos = max 1 pos in
  let prev_ports = ports wld intf dir in
  let prev_ports = List.map
    (fun uri ->
       (port_rank uri, port_type wld uri,
        Misc.opt_of_string (port_name wld uri))
    )
    prev_ports
  in
  let rec iter inserted m acc = function
    [] ->
      let acc =
        if not inserted then
          (m, filetype, name) :: acc
        else
          acc
      in
      List.rev acc
  | (_, hftype,hname) as h :: q ->
      if pos <= m && not inserted then
        iter true (m+1) ((m, filetype, name)::acc) (h :: q)
      else
        iter inserted (m+1) ((m,hftype,hname) :: acc) q
  in
  let new_ports = iter false 1 [] prev_ports in
  assert (List.length new_ports > List.length prev_ports);
  set_ports wld intf dir new_ports
;;

let string_of_port_type wld = function
  One uri -> Grdf_ftype.extension wld uri
| List uri -> Printf.sprintf "%s list" (Grdf_ftype.extension wld uri)
;;

let string_type_of_ports wld ~sep = function
  [] -> "()"
| l ->
    String.concat sep
    (List.map (fun p -> string_of_port_type wld (port_type wld p)) l)
;;

let dir_of_string = function
  "in" -> In | "out" -> Out
| s -> failwith (Printf.sprintf "Invalid direction %S" s)
;;



  