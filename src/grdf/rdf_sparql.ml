(** *)

type varname = string
type projection = varname list

type var_iri = [ `V of varname | `I of string]
type var_iri_a = [ var_iri | `A ]

type node = [ var_iri | `B_ | `B of varname]
type node_a = [ var_iri_a | `B_ | `B of varname]

type triple_obj = [  node | `T of triple_pred list]
and pred_node = [ node_a | `T of triple_pred list ]
and triple_pred = pred_node * triple_obj list

type triple = [ node | `T of triple_pred list ] * triple_pred list

type group_or_union_graph_pattern =
  [ `Group of group_graph_pattern
  | `Union of group_graph_pattern * group_graph_pattern ]

and group_pattern_not_triples =
  [ `Optional of group_graph_pattern
  | `Group of group_graph_pattern
  | `Union of group_graph_pattern * group_graph_pattern
  | `Graph of var_iri * group_graph_pattern ]

and filter = string
and group_graph_pattern =
  triple list *
  [ `Optional of group_graph_pattern
  | `Group of group_graph_pattern
  | `Union of group_graph_pattern * group_graph_pattern
  | `Graph of var_iri * group_graph_pattern
  | `Filter of filter ] option

type select_query =
  { select_proj : projection ;
    select_distinct : [ `Distinct | `Reduced ] option ;
    select_where : group_graph_pattern ;
  }
type construct_query = string
type ask_query = string
type describe_query = string

type query =
  Select of select_query
| Construct of construct_query
| Ask of ask_query
| Describe of describe_query

let string_of_var_iri = function
  `V var -> Printf.sprintf "?%s" var
| `I iri -> Printf.sprintf "<%s>" iri
;;

let string_of_var_iri_a = function
  `A -> "a"
| (`V _) | (`I _) as k -> string_of_var_iri k
;;

let string_of_projection l = String.concat " " (List.map ((^) "?") l);;

let rec string_of_node = function
  | (`V _) | (`I _) | `A as v -> string_of_var_iri_a v
  | `B_ -> "[]"
  | `B s -> Printf.sprintf "_:%s" s
  | `T preds ->
    Printf.sprintf "[ %s ]" (string_of_triple_preds preds)

and string_of_triple_preds (preds : triple_pred list) =
  String.concat " ; "
  (List.map
   (fun (pred, objs) ->
      Printf.sprintf "%s %s" (string_of_node pred) (string_of_triple_objs objs)
   )
   preds
  )

and string_of_triple_objs objs =
  String.concat " , "
  (List.map string_of_node (objs :> pred_node list))
;;

let string_of_triple (sub, preds)=
  Printf.sprintf "%s %s"
  (string_of_node (sub :> pred_node))
  (string_of_triple_preds preds)
;;

let string_of_triples (l : triple list) =
  String.concat " . " (List.map string_of_triple l)
;;

let rec string_of_ggp (triples, rest) =
  match triples, rest with
    [], None -> ""
  | triples, None -> string_of_triples triples
  | triples, Some rest ->
      let s_triples =
        match triples with [] -> "" | _ -> (string_of_triples triples)^" "
      in
      let s2 =
        match rest with
          `Optional ggp -> Printf.sprintf "OPTIONAL { %s }" (string_of_ggp ggp)
        | `Group ggp -> Printf.sprintf "{ %s }" (string_of_ggp ggp)
        | `Union (ggp1, ggp2) ->
            Printf.sprintf "{ %s } UNION { %s }" (string_of_ggp ggp1) (string_of_ggp ggp2)
        | `Graph (var_iri, ggp) ->
            Printf.sprintf "GRAPH %s { %s }" (string_of_var_iri var_iri) (string_of_ggp ggp)
        | `Filter filter ->
            Printf.sprintf "FILTER %s" filter
      in
      Printf.sprintf "%s %s" s_triples s2
;;

let string_of_select q =
  Printf.sprintf "SELECT %s%s WHERE { %s }"
  (match q.select_distinct with
     None -> ""
   | Some `Distinct -> "DISTINCT "
   | Some `Reduced -> "REDUCED ")
  (string_of_projection q.select_proj)
  (string_of_ggp q.select_where)
;;

let string_of_construct q = ""
let string_of_ask q = ""
let string_of_describe q = ""

let string_of_query = function
| Select q -> string_of_select q
| Construct q -> string_of_construct q
| Ask q -> string_of_ask q
| Describe q -> string_of_describe q
;;

let select_and_fold world model query f =
  let query = string_of_select query in
  let q = Rdf_query.new_query ~name: "sparql" world ~query in
  try
    let qr = Rdf_model.query_execute model q in
    let rec iter acc =
      if Rdf_query_results.finished qr then
        List.rev acc
      else
        (
         let acc = f acc qr in
         ignore(Rdf_query_results.next qr);
         iter acc
        )
    in
    iter []
  with
    Rdf_query_results.Query_results_creation_failed _ ->
      failwith ("Query failed: "^query)
;;
