(** *)
open Grdf_types;;

type port = One of uri | List of uri;;
type dir = In | Out ;;

let pred_of_dir = function
| In -> Grdfs.genet_consumes
| Out -> Grdfs.genet_produces
;;

let ports wld dir uri =
  let pred = pred_of_dir dir in
  let f acc n node =
    match Grdfs.target_uri wld node Grdfs.genet_listof with
      None ->
        begin
          match Rdf_node.get_uri node with
            None -> acc
          | Some uri -> (n, One (Rdf_uri.as_string uri)) :: acc
        end
    | Some uri -> (n, List uri) :: acc
  in
  let l = Grdfs.fold_target_sequence f [] wld ~source: uri ~pred in
  List.sort (fun (n1, _) (n2, _) -> Pervasives.compare n1 n2) l
;;

let insert_port wld seq_node (n, port) =
  let world = wld.wld_world in
  let li = Rdf_node.new_from_uri_string world (Grdfs.li_ n) in
  match port with
    One uri ->
      let obj = Rdf_node.new_from_uri_string world uri in
      Grdfs.add_stmt world wld.wld_model
        ~sub: seq_node ~pred: li ~obj
  | List uri ->
      let obj = Rdf_node.new_from_blank_identifier world in
      Grdfs.add_stmt world wld.wld_model
        ~sub: seq_node ~pred: li ~obj;
      let ftype = Rdf_node.new_from_uri_string world uri in
      let pred = Rdf_node.new_from_uri_string world Grdfs.genet_listof in
      Grdfs.add_stmt world wld.wld_model
        ~sub: obj ~pred ~obj: ftype
;;

let delete_ports wld dir uri =
  let pred = pred_of_dir dir in
  let query =
    let triples1 = [
        (`I uri, [ `I pred, [`V "seq"] ] );
        (`V "seq", [ `V "seq_index", [`V "uri"] ])
      ]
    in
    let triples2 =
      [ (`V "uri", [ `I Grdfs.genet_listof, [ `V "uri2"] ]) ]
    in
    let where =
      (triples1, Some (`Optional (triples2, None)))
    in
    { Rdf_sparql.select_proj = [ "seq" ; "seq_index" ; "uri" ; "uri2" ] ;
      select_distinct = None ;
      select_where = where;
    }
  in
  let get = Rdf_query_results.get_binding_value in
  let sub = Rdf_node.new_from_uri_string wld.wld_world uri in
  let pred = Rdf_node.new_from_uri_string wld.wld_world pred in
  let pred_listof = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_listof in
  let f () qr =
    match get qr 0, get qr 1, get qr 2 with
      None, _, _ | _, None, _ | _, _, None -> ()
    | Some seq, Some seq_index, Some uri ->
        Grdfs.remove_stmt wld.wld_world wld.wld_model
          ~sub ~pred ~obj: seq;
        Grdfs.remove_stmt wld.wld_world wld.wld_model
          ~sub: seq ~pred: seq_index ~obj: uri;
        match Rdf_node.kind uri with
        | Rdf_node.Blank _ ->
            begin
              match get qr 4 with
                None -> ()
              | Some obj ->
                  Grdfs.remove_stmt wld.wld_world wld.wld_model
                  ~sub: uri ~pred: pred_listof ~obj
            end
        | _ -> ()
  in
  Rdf_sparql.select_and_fold wld.wld_world wld.wld_model query f ()
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
let set_ports wld dir uri ports =
  let pred = pred_of_dir dir in
  delete_ports wld dir uri ;
  let source = Rdf_node.new_from_uri_string wld.wld_world uri in
  let obj = Rdf_node.new_from_blank_identifier wld.wld_world in
  let pred = Rdf_node.new_from_uri_string wld.wld_world pred in
  Grdfs.add_stmt wld.wld_world wld.wld_model
    ~sub: source ~pred ~obj;
  List.iter (insert_port wld obj) ports
;;

let add_port wld dir uri ?(pos=max_int) filetype =
  let ports = ports wld dir uri in
  let rec iter inserted m acc = function
    [] ->
      let acc =
        if not inserted then
         (m, filetype) :: acc
        else
          acc
      in
      List.rev acc
  | h :: q ->
      if pos <= m && not inserted then
        iter true (m+1) ((m, filetype)::acc) (h :: q)
      else
        iter inserted (m+1) (h :: acc) q
  in
  let new_ports = iter false 1 [] ports in
  set_ports wld dir uri new_ports
;;

let string_of_port wld = function
  One uri -> Grdf_ftype.extension wld uri
| List uri -> Printf.sprintf "%s list" (Grdf_ftype.extension wld uri)
;;