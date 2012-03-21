(** Chain parser *)

%{
open Chn_ast
%}
%token <string> Ident
%token <string> CapIdent
%token <string> String
%token <string> Comment
%token <int> Int

%token DOT
%token LBRACE RBRACE
%token COLON
%token SEMICOLON
%token COMMA
%token RIGHTARROW

%token CHAIN OPERATION
%token IN OUT

%token EOF

%start <Chn_ast.ast> ast

%%

%public ast: option(Comment) chains=list(chain) option(EOF) { chains }

chain: option(Comment) CHAIN ident=Ident comment=Comment LBRACE body=chain_body RBRACE
  {
    let (inputs, outputs, ops, edges) = body in
    let start = $startpos(ident) in
    let stop = $endpos(body) in
    let loc =
      { Loc.loc_start = start ;
        Loc.loc_end = stop ;}
    in
    { chn_name = Chn_types.chain_basename_of_string ident ;
      chn_loc = loc ;
      chn_comment = comment ;
      chn_inputs = inputs ;
      chn_outputs = outputs ;
      chn_ops = ops ;
      chn_edges = edges ;
    }
  }

chain_body: ins=inputs outs=option(outputs) ops=list(operation) edges=list(edge)
  {
    (Array.of_list ins,
     (match outs with None -> [| |] | Some l -> Array.of_list l),
     ops,
     edges
    )
  }

inputs: IN COLON separated_nonempty_list(COMMA, port) SEMICOLON option(Comment) { $3 }
outputs: OUT COLON separated_nonempty_list(COMMA, port) SEMICOLON option(Comment) { $3 }

port: ftype=Ident name=Ident {
  let start = $startpos(ftype) in
  let stop = $endpos(name) in
  let loc =
      { Loc.loc_start = start ;
        Loc.loc_end = stop ;}
  in
  { p_name = name ;
    p_loc = loc ;
    p_ftype = ftype ;
  }
}

operation: OPERATION ident=Ident COLON from=op_origin SEMICOLON option(Comment)
  {
  let start = $startpos(ident) in
  let stop = $endpos(from) in
  let loc =
      { Loc.loc_start = start ;
        Loc.loc_end = stop ;}
  in
  let start_from = $startpos(from) in
  let loc_from =
      { Loc.loc_start = start_from ;
        Loc.loc_end = stop ;}
  in
  { op_name = ident ;
    op_loc = loc ;
    op_from = from ;
    op_from_loc = loc_from ;
  }
}

op_origin:
| s=String { Interface s}
| name=chain_fullname { Chain name }

chain_fullname:
| modname=chain_modname DOT ident=Ident {
    let modname = Chn_types.chain_modname_of_string (String.concat "." modname) in
  let basename = Chn_types.chain_basename_of_string ident in
    Chn_types.mk_chain_name modname basename
 }

chain_modname:
| h=chain_modname DOT q=CapIdent { h @ [q] }
| q=CapIdent { [q] }

edge: src=edge_part RIGHTARROW dst=edge_part SEMICOLON option(Comment)
  {
   { edge_src = src ; edge_dst = dst }
  }

edge_part:
| op=Ident DOT p=port_ref {
  let start = $startpos(op) in
  let stop = $endpos(p) in
  let loc =
    { Loc.loc_start = start ;
      Loc.loc_end = stop ;}
  in
  { ep_op = Some op ; ep_port = p ; ep_loc = loc }
  }
| p=port_ref {
  let start = $startpos(p) in
  let stop = $endpos(p) in
  let loc =
    { Loc.loc_start = start ;
      Loc.loc_end = stop ;}
  in
  { ep_op = None ; ep_port = p ; ep_loc = loc }
  }

port_ref:
| n=Int { Pint n }
| ident=Ident { Pname ident }

