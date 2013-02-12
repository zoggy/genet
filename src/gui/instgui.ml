(** Gui elements to display instanciated chains. *)


class inst_chain_box ctx =
  let vbox = GPack.vbox () in
  object(self)
    method coerce = vbox#coerce
  end
;;

class box ctx =
  let paned = GPack.paned `VERTICAL () in
  let hbox = GPack.hbox () in
  let diffbox = GPack.vbox () in
  let instbox1 = new inst_chain_box ctx in
  let instbox2 = new inst_chain_box ctx in
  object(self)

    method coerce = paned#coerce

    initializer
      hbox#pack ~expand: true ~fill: true instbox1#coerce;
      hbox#pack ~expand: true ~fill: true instbox2#coerce;
      paned#add1 hbox#coerce ;
      paned#add2 diffbox#coerce ;
      paned#set_position 400;
  end;;
  