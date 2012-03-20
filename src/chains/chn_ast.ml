(** *)

type operation_name = string
type port_name = string

type port = {
  p_name : port_name ;
  p_loc : Loc.t ;
  p_ftype : string ;
}


type port_ref = Pint of int | Pname of port_name
type edge_part = {
  ep_op : operation_name option ;
  ep_port : port_ref ;
  ep_loc : Loc.t ;
  }

type edge = {
    edge_src : edge_part ;
    edge_dst : edge_part ;
  }

type 'a operation = {
  op_name : operation_name ;
  op_loc : Loc.t ;
  op_from : 'a ;
  op_from_loc : Loc.t ;
}

type 'a chain = {
  chn_name : string ;
  chn_loc : Loc.t ;
  chn_comment : string ;
  chn_inputs : port array ;
  chn_outputs : port array ;
  chn_ops : 'a operation list ;
  chn_edges : edge list ;
}

type 'a ast = 'a chain list;;

class ['a] ast_printer (f_from : 'a -> string) =
  object(self)

    method string_of_port p = Printf.sprintf "%s %s" p.p_ftype p.p_name

    method string_of_port_array l =
      let l = Array.to_list l in
      String.concat ", " (List.map self#string_of_port l)

    method string_of_operation op =
      Printf.sprintf "  operation %s : %s ;\n" op.op_name (f_from op.op_from)

    method string_of_operation_list l =
      String.concat "" (List.map self#string_of_operation l)

    method string_of_port_ref = function
      Pint n -> string_of_int n
    | Pname s -> s

    method string_of_edge e =
      Printf.sprintf "  %s%s -> %s%s ;\n"
        (match e.edge_src.ep_op with None -> "" | Some s -> s^".")
        (self#string_of_port_ref e.edge_src.ep_port)
        (match e.edge_dst.ep_op with None -> "" | Some s -> s^".")
        (self#string_of_port_ref e.edge_dst.ep_port)

    method string_of_edge_list l =
      String.concat "" (List.map self#string_of_edge l)

    method string_of_chain chn =
      let b = Buffer.create 256 in
      Printf.bprintf b "chain %s\n(* %s *)\n{\n" chn.chn_name chn.chn_comment ;
      Printf.bprintf b "  in: %s ;\n" (self#string_of_port_array chn.chn_inputs) ;
      Printf.bprintf b "  out: %s ;\n" (self#string_of_port_array chn.chn_outputs) ;
      Printf.bprintf b "\n%s" (self#string_of_operation_list chn.chn_ops);
      Printf.bprintf b "\n%s" (self#string_of_edge_list chn.chn_edges);
      Buffer.add_string b "}\n";
      Buffer.contents b

    method string_of_ast l =
      String.concat "\n" (List.map self#string_of_chain l)

  end

let raw_printer () = new ast_printer (fun s -> Printf.sprintf "%S" s);;

