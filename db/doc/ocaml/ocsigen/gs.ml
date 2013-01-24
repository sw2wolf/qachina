open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml

(*
 #use"topfind";;
 #require"mysql";;
 #require"str";;
*)

open Str
open Mysql

module Ep = Eliom_predefmod;;


let trim the_input =
    let space_regexp = Str.regexp " +"
    and semicolumn_regexp = Str.regexp ";"
    and replace = Str.global_replace
    in replace semicolumn_regexp ""        (* Remplacement des point-virgules *)
       (replace space_regexp "" the_input);;    (* et des espaces. anti-problemes  *)

let semicolumn_regexp = Str.regexp ";";;

let ok_credentials (u,p) = match (u,p) with
    | ("admin","sdlqskd6dsf") -> true;
    | _                       -> false;;

(* begin HTML constructions *)
let greeter_auth close_serv server_params (login, password) =
    p [pcdata ("Bienvenue, titulaire de l'adresse "^login^".");
       br ();
       (Eliom_predefmod.Xhtml.a
        close_serv
        server_params 
        [pcdata "Se déconnecter."] ());
      ] ;;

let login_page attached_serv server_params titre= 
    let login_form =
        Eliom_predefmod.Xhtml.post_form
        attached_serv
        server_params
        (fun (login, password) ->
            [p [pcdata "Identifiant:";
             (Eliom_predefmod.Xhtml.string_input ~input_type:`Text ~name:login ());
             br ();
             pcdata "Mot de passe";
             (Eliom_predefmod.Xhtml.string_input ~input_type:`Text ~name:password ());
             br ();
             (Eliom_predefmod.Xhtml.string_input ~value:"S'identifier" ~input_type:`Submit ())]]) ()
    in
    (html
        (head (title (pcdata titre)) [])
        (body
        [login_form]));;

(* end HTML constructions *)
