(*********************************************************************************)
(*                Genet                                                          *)
(*                                                                               *)
(*    Copyright (C) 2012 Institut National de Recherche en Informatique          *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License version 3             *)
(*    or later as published by the Free Software Foundation.                     *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software Foundation,    *)
(*    Inc., 59 Temple Place, Suite 330, Boston, MA                               *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(** *)

type varname = string
type projection = varname list

(** Variant constructors signification:
- [`V]: Variable. The given name must no include the "?".
- [`I]: IRI. The string must not include the starting '<' and ending '>'.
- [`B_]: Blank node.
- [`B]: Blank node with an identifier. The given string must no
  include the starting "_:"
- [`T]: Triple.
- [`L]: Literal. String literals must contains quotes, as
  nothing is added to the given literal string.
*)

type var_iri = [ `V of varname | `I of string]
type var_iri_a = [ var_iri | `A ]

type node_obj = [ var_iri | `B_ | `B of varname | `L of string]
type node = [ var_iri | `B_ | `B of varname ]
type node_a = [ var_iri_a | `B_ | `B of varname]

type triple_obj = [  node_obj | `T of triple_pred list]
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

and quads_pattern = quads
and quads = triples_template option * (quads_not_triples * triples_template option) list

and quads_not_triples = var_iri * triples_template option

and triples_template = triple list (* even though this must not be an empty list *)

type select_query =
  { select_proj : projection ;
    select_distinct : [ `Distinct | `Reduced ] option ;
    select_where : group_graph_pattern ;
  }
type construct_query =
  { construct_triples : triple list ;
    construct_where : group_graph_pattern ;
  }

type ask_query = string
type describe_query = string

type delete_where_query = quads_pattern

type delete_insert_query =
  { delins_delete : quads_pattern option ;
    delins_insert : quads_pattern option ;
    delins_where : group_graph_pattern ;
  }

type query =
  Select of select_query
| Construct of construct_query
| Ask of ask_query
| Describe of describe_query
| Delete_where of delete_where_query
| Delete_insert of delete_insert_query

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

and string_of_node_obj = function
| `L s -> s
| (`V _) | (`I _) as v -> string_of_var_iri v
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
  (List.map string_of_node_obj objs)
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

and string_of_quads_pattern quads =
  Printf.sprintf "{ %s }" (string_of_quads quads)

and string_of_quads =
 let f = function
   | qnt, None -> string_of_quads_not_triples qnt
   | qnt, Some tpl_tmpl ->
     Printf.sprintf "%s . %s"
       (string_of_quads_not_triples qnt)
       (string_of_triples_template tpl_tmpl)
 in
 function
      | (None, l) -> String.concat " " (List.map f l)
      | (Some tpl_tmpl, l) ->
          Printf.sprintf "%s %s"
            (string_of_triples_template tpl_tmpl)
            (String.concat " " (List.map f l))

and string_of_triples_template l =
  String.concat " . " (List.map string_of_triple l)

and string_of_quads_not_triples (var_iri, tpl_tmpl) =
  Printf.sprintf "GRAPH %s { %s }"
  (string_of_var_iri var_iri)
  (match tpl_tmpl with
     None -> ""
   | Some tpl_tmpl -> string_of_triples_template tpl_tmpl)
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

let string_of_construct q =
  Printf.sprintf "CONSTRUCT { %s } WHERE { %s }"
  (string_of_triples q.construct_triples)
  (string_of_ggp q.construct_where)
;;

let string_of_ask q = ""
let string_of_describe q = ""

let string_of_delete_where q =
  Printf.sprintf "DELETE WHERE %s" (string_of_quads_pattern q)

let string_of_delete_insert q =
  Printf.sprintf "%s%sWHERE %s"
  (match q.delins_delete with
     None -> ""
   | Some q -> Printf.sprintf "DELETE %s " (string_of_quads_pattern q)
  )
  (match q.delins_insert with
     None -> ""
   | Some q -> Printf.sprintf "INSERT %s " (string_of_quads_pattern q)
  )
  (string_of_ggp q.delins_where)
;;

let string_of_query = function
| Select q -> string_of_select q
| Construct q -> string_of_construct q
| Ask q -> string_of_ask q
| Describe q -> string_of_describe q
| Delete_where q -> string_of_delete_where q
| Delete_insert q -> string_of_delete_insert q
;;

let exec world model query =
  let query = string_of_query query in
  (*prerr_endline (Printf.sprintf "exec query=%s" query);*)
  (*Rdf_query.new_query ~name: "sparql" world ~query*)
  failwith ("Query not implemented: "^query)
;;

let select_and_fold world model query f acc =
  let query = string_of_select query in
  (*prerr_endline query;*)
  failwith ("query not implemented: "^query)
  (*
  let q = Rdf_query.new_query ~name: "sparql" world ~query in
  try
    let qr = Rdf_model.query_execute model q in
    let rec iter acc =
      let finished = Rdf_query_results.finished qr in
      (*prerr_endline (Printf.sprintf "coucou, finished=%b" finished);*)
      if finished then
        acc
      else
        (
         let acc = f acc qr in
         ignore(Rdf_query_results.next qr);
         iter acc
        )
    in
    iter acc
  with
    Rdf_query_results.Query_results_creation_failed _ ->
      failwith ("Query failed: "^query)*)
;;

let exec_construct world model query =
  let query = string_of_construct query in
  failwith ("query not implemented: "^query)
  (*let q = Rdf_query.new_query ~name: "sparql" world ~query in
  try
    let qr = Rdf_model.query_execute model q in
    if Rdf_query_results.is_graph qr then
      Rdf_query_results.as_stream qr
    else
      assert false
  with
    Rdf_query_results.Query_results_creation_failed _ ->
      failwith ("Query failed: "^query)
      *)
;;
