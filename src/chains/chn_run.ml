(** Running commands to instanciate a chain. *)

module Graph = Graph.Make_with_map
  (struct
     type t = Rdf_uri.uri
     let compare = Rdf_uri.compare
   end
  )
  (struct
     type t = Rdf_uri.uri * Rdf_uri.uri
     let compare (p1, p2) (p3, p4) =
       match Rdf_uri.compare p1 p3 with
         0 -> Rdf_uri.compare p2 p4
       | n -> n
   end)
;;

let run_graph ctx reporter uri_fchain g port_to_file =
  print_endline "run_graph"
;;
