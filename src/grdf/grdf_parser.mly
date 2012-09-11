(** Type parser *)

%start <string Grdf_types.port_type> port_type

%%

port_type: t=the_port_type option(EOF) { t }

the_port_type:
  s=Ident { Grdf_types.T s }
| s=Var { Grdf_types.Var s }
| t=the_port_type SET { Grdf_types.Set t }
| LPAR l=separated_nonempty_list (STAR, the_port_type) RPAR { Grdf_types.Tuple l }
