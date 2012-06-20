(** Reporter. Reporting error and messages in multiple treatments. *)

type msg =
  | Error of string
  | Msg of string
  | Context of string * int * msg list
;;

class reporter ?(context="") verb_level =
  object(self)
    val mutable total_errors = 0
    val mutable contexts =
      let t = Stack.create () in
      Stack.push (context, 0, []) t;
      t

    method error s =
      let (label, errors, l) = Stack.pop contexts in
      let l = (Error s) :: l in
      Stack.push (label, errors, l) contexts;

    method msg ?(level=0) s =
      if level <= verb_level then
        begin
          let (label, errors, l) = Stack.pop contexts in
          let l = (Msg s) :: l in
          Stack.push (label, errors, l) contexts
        end
    method incr_errors =
      total_errors <- total_errors + 1;
      let (label, errors, l) = Stack.pop contexts in
      Stack.push (label, errors + 1, l) contexts

    method push_context label =
      Stack.push (label, 0, []) contexts

    method pop_context =
      let (label, n, l) =
        try Stack.pop contexts
        with _ -> assert false
      in
      try
        let (label2, n2, l2) = Stack.pop contexts in
        Stack.push (label2, n2 + n, (Context (label, n, l)) :: l) contexts
      with
        Stack.Empty ->
          Stack.push (label, n, l) contexts;
          failwith "No more context to pop !"

    method messages =
      let t = Stack.copy contexts in
      try
        while true do self#pop_context done; assert false
      with
        Failure _ ->
          let (_,_,l) = Stack.pop contexts in
          contexts <- t;
          l

    method total_errors = total_errors
  end

let rec string_of_msg pad label = function
  Error msg -> Printf.sprintf "%s%s[Error] %s"
    pad (match label with "" -> "" | _ -> label^" ") msg
| Msg msg -> Printf.sprintf "%s%s %s" pad label msg
| Context (label,_,l) ->
    string_of_msg_list ~pad: (pad^"  ") ~label l

and string_of_msg_list ?(pad="") ?(label="") l =
  let l = List.rev_map (string_of_msg pad label) l in
  String.concat "\n" l
;;
