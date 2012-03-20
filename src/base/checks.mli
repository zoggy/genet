(** Utilitaires pour les contrôles et les vérifications. *)

(** Affichage d'un warning. Par défaut, un warning est affiché sur
   la sortie d'erreur standard.*)
val set_print_warning_fun : (string -> unit) -> unit
val print_warning : string -> unit

(** Représentation d'un problème, indiquant l'élément en question
   et la cause du problème. On a également éventuellement l'exception
   source du problème. *)
type 'a problem = private { pb_ref : 'a; pb_msg : string; pb_exn : exn option }

(** Création d'un problème. *)
val problem : ?exn: exn -> 'a -> string -> 'a problem

(** Représentation du résultat d'une vérification: une liste d'erreurs
     et une liste d'avertissements. *)
type 'a check_result = {
  chk_errors : 'a problem list;
  chk_warnings : 'a problem list;
}

(** Création d'un résultat de vérification. *)
val check_result : ?warns: 'a problem list -> 'a problem list -> 'a check_result

(** Aucun problème. *)
val empty_check_result : 'a check_result

(** Test si aucun problème ni warning. *)
val is_empty : 'a check_result -> bool

(** Fusion de deux résultats de vérification. *)
val merge_check_result : 'a check_result -> 'a check_result -> 'a check_result

(** Fonction facilitant l'enchaînement de plusieurs vérifications.
     @acc peut être donné pour ne pas commencer avec un résultat vide.
*)
val fold_check :
  ?acc:'a check_result -> ('b -> 'a check_result) -> 'b list -> 'a check_result

(** Obtention d'une chaîne pour afficher un problème.
    La fonction en argument permet d'obtenir une chaîne pour la référence
    du problème.
*)
val string_of_problem : ('a -> string) -> 'a problem -> string

(** Obtention d'une chaîne pour afficher les erreurs et avertissements
    du résultat donné.
    La fonction en argument permet d'obtenir une chaîne pour les références
    des problèmes.
*)
val string_of_check_result : ('a -> string) -> 'a check_result -> string
