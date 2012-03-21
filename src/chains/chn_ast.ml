(** *)

open Chn_types;;

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

type op_origin = Chain of chain_name | Interface of string

type operation = {
  op_name : operation_name ;
  op_loc : Loc.t ;
  op_from : op_origin ;
  op_from_loc : Loc.t ;
}

type chain = {
  chn_name : chain_basename ;
  chn_loc : Loc.t ;
  chn_comment : string ;
  chn_inputs : port array ;
  chn_outputs : port array ;
  chn_ops : operation list ;
  chn_edges : edge list ;
}

type ast = chain list;;

let get_chain ast basename =
  try
    let chn =
      List.find
      (fun chn -> Chn_types.compare_chain_basename chn.chn_name basename = 0)
      ast
    in
    Some chn
  with
    Not_found -> None
;;

class ast_printer =
  object(self)
    method string_of_port p = Printf.sprintf "%s %s" p.p_ftype p.p_name

    method string_of_port_array l =
      let l = Array.to_list l in
      String.concat ", " (List.map self#string_of_port l)

    method string_of_op_origin = function
      Chain s -> Chn_types.string_of_chain_name s
    | Interface uri -> Printf.sprintf "%S" uri

    method string_of_operation op =
      Printf.sprintf "  operation %s : %s ;\n" op.op_name
      (self#string_of_op_origin op.op_from)

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
      Printf.bprintf b "chain %s\n(* %s *)\n{\n"
      (Chn_types.string_of_chain_basename chn.chn_name) chn.chn_comment ;
      Printf.bprintf b "  in: %s ;\n" (self#string_of_port_array chn.chn_inputs) ;
      Printf.bprintf b "  out: %s ;\n" (self#string_of_port_array chn.chn_outputs) ;
      Printf.bprintf b "\n%s" (self#string_of_operation_list chn.chn_ops);
      Printf.bprintf b "\n%s" (self#string_of_edge_list chn.chn_edges);
      Buffer.add_string b "}\n";
      Buffer.contents b

    method string_of_ast l =
      String.concat "\n" (List.map self#string_of_chain l)

  end

let printer () = new ast_printer ;;

module Dot =
  struct
    let color_in = "palegreen"
    let color_out = "paleturquoise"
    let color_chain = "grey75"
    let color_interface = "lightsalmon"

    let string_of_port_ref ?(label=false) = function
      Pint n -> if label then string_of_int n else Printf.sprintf "p%d" n
    | Pname s -> s

    let string_of_op_origin = function
      Chain s -> Chn_types.string_of_chain_name s
    | Interface uri -> Printf.sprintf "%S" uri

    let uri_of_op_origin prefix = function
      Chain s -> Chn_types.uri_chain prefix s
    | Interface uri -> Chn_types.uri_intf_of_interface_spec ~prefix uri

    let color_of_op_origin = function
      Chain _ -> color_chain
    | Interface _ -> color_interface

    let print_operation ?prefix b chn op =
      let f map acc edge =
        let ep = map edge in
        match ep.ep_op with
        | Some name when name = op.op_name ->
            let p = ep.ep_port in
            if List.mem p acc then acc else p :: acc
        | _-> acc
      in
      let get_ports map = List.fold_left (f map) [] chn.chn_edges in
      let inputs = get_ports (fun edge -> edge.edge_dst) in
      let outputs = get_ports (fun edge -> edge.edge_src) in
      let string_of_port color p =
        Printf.sprintf "<TR><TD BGCOLOR=\"%s\" PORT=\"%s\">%s</TD></TR>"
        color (string_of_port_ref p) (string_of_port_ref ~label: true p)
      in
      let bgcolor = color_of_op_origin op.op_from in

      let string_of_ports color ports =
        Printf.sprintf  "<TD><TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">%s</TABLE></TD>"
        (String.concat "" (List.map (string_of_port color) ports))
      in
      Printf.bprintf b "%s [ shape=plaintext label=<<TABLE BGCOLOR=\"%s\" BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\" CELLPADDING=\"3\"><TR>" op.op_name bgcolor;
      Printf.bprintf b "%s" (string_of_ports color_in inputs);
      Printf.bprintf b "<TD ALIGN=\"CENTER\" HREF=\"%s\" CELLPADDING=\"4\">%s</TD>"
      (match prefix with None -> "" | Some prefix -> uri_of_op_origin prefix op.op_from)
      (string_of_op_origin op.op_from);
      Printf.bprintf b "%s" (string_of_ports color_out outputs);
      Buffer.add_string b "</TR></TABLE>>];\n"
    let string_of_edge_part ep =
      match ep.ep_op with
        None -> string_of_port_ref ep.ep_port
      | Some op_name -> Printf.sprintf "%s:%s" op_name (string_of_port_ref ep.ep_port)

    let print_edge b edge =
      Printf.bprintf b "%s -> %s ;\n"
      (string_of_edge_part edge.edge_src)
      (string_of_edge_part edge.edge_dst)

    let print_port b color p =
      Printf.bprintf b "%s [color=\"black\" fillcolor=\"%s\" style=\"filled\" shape=\"box\" label=\"%s\"];\n"
        p.p_name color p.p_name

    let dot_of_chain ?prefix chain =
      let b = Buffer.create 256 in
      Buffer.add_string b "digraph g {\nrankdir=LR;\nfontsize=10;\n";
      List.iter (print_operation ?prefix b chain) chain.chn_ops;
      List.iter (print_edge b) chain.chn_edges;
      Array.iter (print_port b color_in) chain.chn_inputs;
      Array.iter (print_port b color_out) chain.chn_outputs;
      Buffer.add_string b "}\n";
      Buffer.contents b
  end;;
