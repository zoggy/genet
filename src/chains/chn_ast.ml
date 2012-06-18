(** *)

open Chn_types;;

type operation_name = string
type port_name = string

type port = {
  p_name : port_name ;
  p_loc : Loc.t ;
  p_ftype : string Grdf_port.port_type ;
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

type chn_module = {
  cmod_name : Chn_types.chain_modname ;
  cmod_chains : chain list ;
  }

let get_chain cmod basename =
  try
    let chn =
      List.find
      (fun chn -> Chn_types.compare_chain_basename chn.chn_name basename = 0)
      cmod.cmod_chains
    in
    Some chn
  with
    Not_found -> None
;;

module Chn_ord_type =
  struct
     type t = Chn_types.chain_name
     let compare = Chn_types.compare_chain_name
   end;;

module Cset = Set.Make(Chn_ord_type);;
module Cmap = Map.Make(Chn_ord_type);;

type chain_deps = {
  dep_chn : chain ;
  dep_intfs : Sset.t ; (* interfaces (in the form "/tool/intf") a chain depends on *)
  dep_chains : Cset.t ; (* chains a chain depends on *)
  dep_unknown : op_origin list ; (* unknown origins the chain refers to *)
};;
let empty_chain_deps chn =
  { dep_chn = chn ;
    dep_intfs = Sset.empty ;
    dep_chains = Cset.empty ;
    dep_unknown = [] ;
  };;

type deps = chain_deps Cmap.t ;;

let compute_deps wld config cmods =
  let f_chn modname map chn =
    Cmap.add
    (Chn_types.mk_chain_name modname chn.chn_name) (empty_chain_deps chn)
    map
  in
  let f_cmod map cmod =
    List.fold_left (f_chn cmod.cmod_name) map cmod.cmod_chains
  in
  let map = List.fold_left f_cmod Cmap.empty cmods in

  let f_origin d op =
    match op.op_from with
    | Interface s ->
        begin
          try
            let uri = Chn_types.uri_intf_of_interface_spec ~prefix: config.Config.rest_api s in
            match Grdf_intf.intf_exists wld uri with
              Some _ -> { d with dep_intfs = Sset.add s d.dep_intfs }
            | None -> raise Not_found
          with
            _ ->
            { d with dep_unknown = (Interface s) :: d.dep_unknown }
        end
    | Chain fullname ->
        try
          ignore(Cmap.find fullname map);
          { d with dep_chains = Cset.add fullname d.dep_chains }
        with Not_found ->
            { d with dep_unknown = (Chain fullname) :: d.dep_unknown }
  in
  let f_chn fullname d map =
    let d = List.fold_left f_origin d d.dep_chn.chn_ops in
    Cmap.add fullname d map
  in
  Cmap.fold f_chn map map
;;

class ast_printer =
  object(self)
    method string_of_port_type = Grdf_port.string_of_port_type (fun x -> x)

    method string_of_port p =
      Printf.sprintf "%s %s" (self#string_of_port_type p.p_ftype) p.p_name

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

    method string_of_chn_module cmod =
      String.concat "\n" (List.map self#string_of_chain cmod.cmod_chains)

  end

let printer () = new ast_printer ;;

class chain_dot_printer =
  object(self)
    method color_in = "palegreen"
    method color_out = "paleturquoise"
    method color_chain = "grey75"
    method color_interface = "lightsalmon"

    method string_of_port_ref dir ?(label=false) = function
      Pint n ->
        if label then
          string_of_int n
        else
          Printf.sprintf "%s_p%d"
          (Grdf_port.string_of_dir dir)
          n
    | Pname s -> s

    method string_of_op_origin = function
      Chain s -> Chn_types.string_of_chain_name s
    | Interface uri -> Printf.sprintf "%S" uri

    method uri_of_op_origin prefix = function
      Chain s -> Chn_types.uri_chain prefix s
    | Interface uri -> Chn_types.uri_intf_of_interface_spec ~prefix uri

    method color_of_op_origin = function
      Chain _ -> self#color_chain
    | Interface _ -> self#color_interface

    method string_of_port dir color p =
      Printf.sprintf "<TR><TD BGCOLOR=\"%s\" PORT=\"%s\">%s</TD></TR>"
      color (self#string_of_port_ref dir p) (self#string_of_port_ref dir ~label: true p)

    method print_operation ?prefix b chn op =
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
      let bgcolor = self#color_of_op_origin op.op_from in

      let string_of_ports dir color ports =
        Printf.sprintf  "<TD><TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"3\" CELLPADDING=\"4\">%s</TABLE></TD>"
        (String.concat "" (List.map (self#string_of_port dir color) ports))
      in
      Printf.bprintf b "%s [ shape=plaintext label=<<TABLE BGCOLOR=\"%s\" BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\" CELLPADDING=\"3\"><TR>" op.op_name bgcolor;
      Printf.bprintf b "%s" (string_of_ports Grdf_port.In self#color_in inputs);
      Printf.bprintf b "<TD ALIGN=\"CENTER\" HREF=\"%s\" CELLPADDING=\"4\">%s</TD>"
      (match prefix with
         None -> ""
       | Some prefix -> Rdf_uri.string (self#uri_of_op_origin prefix op.op_from)
      )
      (self#string_of_op_origin op.op_from);
      Printf.bprintf b "%s" (string_of_ports Grdf_port.Out self#color_out outputs);
      Buffer.add_string b "</TR></TABLE>>];\n"

    method string_of_edge_part dir ep =
      match ep.ep_op with
        None -> self#string_of_port_ref dir ep.ep_port
      | Some op_name -> Printf.sprintf "%s:%s" op_name (self#string_of_port_ref dir ep.ep_port)

    method print_edge b edge =
      Printf.bprintf b "%s -> %s ;\n"
      (self#string_of_edge_part Grdf_port.Out edge.edge_src)
      (self#string_of_edge_part Grdf_port.In edge.edge_dst)

    method print_port b color p =
      Printf.bprintf b "%s [color=\"black\" fillcolor=\"%s\" style=\"filled\" shape=\"box\" label=\"%s\"];\n"
        p.p_name color p.p_name

    method dot_of_chain ?prefix chain =
      let b = Buffer.create 256 in
      Buffer.add_string b "digraph g {\nrankdir=LR;\nfontsize=10;\n";
      List.iter (self#print_operation ?prefix b chain) chain.chn_ops;
      List.iter (self#print_edge b) chain.chn_edges;
      Array.iter (self#print_port b self#color_in) chain.chn_inputs;
      Array.iter (self#print_port b self#color_out) chain.chn_outputs;
      Buffer.add_string b "}\n";
      Buffer.contents b

  end;;

class chain_dot_deps ?(chain_dot=new chain_dot_printer) () =
  object(self)
    method id s = Printf.sprintf "%S" s
    method chain_id n = self#id (Chn_types.string_of_chain_name n)
    method intf_id s = self#id (String.concat "___" (Misc.split_string s ['/']))

    method print_dep_chn b id fullname =
      Printf.bprintf b "%s -> %s;\n" id (self#chain_id fullname)

    method print_dep_intf b id s intfs =
      Printf.bprintf b "%s -> %s;\n" id (self#intf_id s);
      Sset.add s intfs

    method print_chn b ~prefix ~fullnames fullname dep intfs =
      let label =
        if fullnames then
          Chn_types.string_of_chain_name fullname
        else
          Chn_types.string_of_chain_basename (Chn_types.chain_basename fullname)
      in
      let id = self#chain_id fullname in
      Printf.bprintf b "%s [label=\"%s\" shape=\"box\" href=\"%s\" style=\"filled\" fillcolor=\"%s\"];\n"
        id label (Rdf_uri.string (Chn_types.uri_chain prefix fullname)) chain_dot#color_chain;
      Cset.iter (self#print_dep_chn b id) dep.dep_chains ;
      let intfs = Sset.fold (self#print_dep_intf b id) dep.dep_intfs intfs in
      intfs

    method print_intf b ~prefix intf =
      let uri = Chn_types.uri_intf_of_interface_spec ~prefix intf in
      Printf.bprintf b
      "%s [label=\"%s\" shape=\"box\" href=\"%s\" style=\"filled\" fillcolor=\"%s\"];\n"
      (self#intf_id intf) intf (Rdf_uri.string uri) chain_dot#color_interface

    method dot_of_deps prefix ?(fullnames=true) deps =
      let b = Buffer.create 256 in
      Buffer.add_string b "digraph g {\nrankdir=TB;\nfontsize=10;\n";
      let intfs = Cmap.fold (self#print_chn b ~prefix ~fullnames) deps Sset.empty in
      Sset.iter (self#print_intf b ~prefix) intfs;
      Buffer.add_string b "}\n";
      Buffer.contents b
  end;;
