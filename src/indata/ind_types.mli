(** Types of input data. *)

type spec = {
    dir : string ; (* absolute directory *)
    in_files : (string * string) list ; (* file * git commit id *)
    out_files : string list ;
    chains : string list ;
  }

