(** Types of input data. *)

type spec = {
    dir : string ; (** absolute directory *)
    from_in_data : string ; (** path from the in/data directory *)
    in_files : (string * string) list ; (** file * git commit id *)
    chains : string list ;
  }

