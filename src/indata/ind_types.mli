(** Types of input data. *)

type spec = {
    dir : string ; (* directory in the in/data directory *)
    in_files : string list ;
    out_files : string list ;
    chains : string list ;
  }

