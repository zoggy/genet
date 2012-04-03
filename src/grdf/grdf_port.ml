(** *)
open Grdf_types;;

type port_type = One of uri | List of uri;;
type dir = In | Out ;;

let pred_of_dir = function
| In -> Grdfs.genet_consumes
| Out -> Grdfs.genet_produces
;;

let port_name wld port = Grdfs.name_of_uri_string wld port;;
let port_rank = Grdfs.port_rank ;;

let port_type wld port =
  let node = Rdf_node.new_from_uri_string wld.wld_world port in
  match Grdfs.target_uri wld node Grdfs.genet_hasfiletype with
    Some uri -> One uri
  | None ->
      match Grdfs.target_uri wld node Grdfs.genet_hasfiletypelist with
        Some uri -> List uri
      | None -> failwith (Printf.sprintf "No type for port %s" port)
;;

let ports wld intf dir =
  let pred = pred_of_dir dir in
  let intf = Rdf_node.new_from_uri_string wld.wld_world intf in
  Grdfs.target_uris wld intf pred
;;

let sort_ports l =
  List.sort (fun p1 p2 ->
    Pervasives.compare (port_rank p1) (port_rank p2))
  l
;;

let unset_port_type wld port =
  let preds = [ Grdfs.genet_hasfiletype ; Grdfs.genet_hasfiletypelist ] in
  let f pred =
    let query =
      let triples = [
          (`I port, [ `I pred, [`V "seq"] ] );
        ]
      in
      (Some triples, [])
    in
    let query = Rdf_sparql.Delete_where query in
    ignore(Rdf_sparql.exec wld.wld_world wld.wld_model query)
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
  let world = wld.wld_world in
  let model = wld.wld_model in
  Grdfs.add_stmt world model
     ~sub: (Rdf_node.new_from_uri_string world port)
     ~pred: (Rdf_node.new_from_uri_string world pred)
     ~obj: (Rdf_node.new_from_uri_string world ftype_uri)
;;

let uri_intf_port_of_dir = function
  In -> Grdfs.uri_intf_in_port
| Out -> Grdfs.uri_intf_out_port
;;

let insert_port wld intf dir (n, typ, name) =
  let pred = pred_of_dir dir in
  let world = wld.wld_world in
  let uri = (uri_intf_port_of_dir dir) intf n in
  let sub = Rdf_node.new_from_uri_string world intf in
  let pred = Rdf_node.new_from_uri_string world pred in
  let obj = Rdf_node.new_from_uri_string world uri in
  Grdfs.add_stmt world wld.wld_model ~sub ~pred ~obj;
  set_port_type wld uri typ;
  match name with None -> () | Some name -> Grdfs.add_name wld sub name
;;

let delete_ports wld uri dir =
  let dir_pred = pred_of_dir dir in
  let f pred =
    let del_triples = [
        (`I uri, [ `I dir_pred, [`V "port"] ] );
        (`V "port", [`I pred, [`V "ftype"] ] );
      ]
    in
    let query = (Some del_triples, []) in
    let query = Rdf_sparql.Delete_where query in
    ignore(Rdf_sparql.exec wld.wld_world wld.wld_model query)
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
  let ports = ports wld intf dir in
  let ports = List.map
    (fun uri ->
       (port_rank uri, port_type wld uri,
        Misc.opt_of_string (port_name wld uri))
    )
    ports
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
  | h :: q ->
      if pos <= m && not inserted then
        iter true (m+1) ((m, filetype, name)::acc) (h :: q)
      else
        iter inserted (m+1) (h :: acc) q
  in
  let new_ports = iter false 1 [] ports in
  set_ports wld intf dir new_ports
;;

let string_of_port_type wld = function
  One uri -> Grdf_ftype.extension wld uri
| List uri -> Printf.sprintf "%s list" (Grdf_ftype.extension wld uri)
;;

let string_type_of_ports wld ~sep = function
  [] -> "()"
| l ->
    let l = sort_ports l in
    String.concat sep
    (List.map (fun p -> string_of_port_type wld (port_type wld p)) l)
;;
