(** Types of input data. *)

type spec = {
    dir : string ; (* absolute directory *)
    in_files : string list ;
    out_files : string list ;
    chains : string list ;
  }

