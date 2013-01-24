open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml

open Gs

module Ep=Eliom_parameters;;

let session_table = Eliom_sessions.create_volatile_table ();;

let root_service =
    Eliom_services.new_service
        ~path:[""]
        ~get_params:Eliom_parameters.unit
        () ;;
                  
let root_service_with_post =
    Eliom_services.new_post_service
        ~fallback:root_service
        ~post_params:((Ep.string "login")**(Ep.string "password"))
        () ;;

let logout_service =
    Eliom_services.new_service
        ~path:["logout"]
        ~get_params:Eliom_parameters.unit
        () ;;

(* Handlers *)
                                                 
let root_handler sp _ _  =
    let session_data = Eliom_sessions.get_volatile_session_data  
        ~table:session_table 
        ~sp:sp () 
    in
    match session_data with 
        Eliom_sessions.Data (name,password) ->
            return
                (html
                (head (title (pcdata "Accueil")) [])
                (body
                [(Gs.greeter_auth  logout_service sp (name,password))])) ;
        | Eliom_sessions.Data_session_expired
        | Eliom_sessions.No_data ->
            return (Gs.login_page root_service_with_post sp "Accueil")  ;;

let root_handler_with_post sp _ (login,password) =
    let clean_login = Gs.trim login 
    in
    match (Gs.ok_credentials (clean_login, password)) with
        | true ->
            Lwt.bind
                (Eliom_sessions.close_session  ~sp:sp ())
                (fun () ->
                    Eliom_sessions.set_volatile_session_data
                        ~table:session_table ~sp:sp (clean_login, password);
                    return
                    (html
                    (head (title (pcdata "Identifié")) [])
                    (body
                    [Gs.greeter_auth logout_service sp (clean_login,password)]))) ;
        | false -> 
            return (Gs.login_page root_service_with_post sp "Recommencez SVP");;

let logout_handler sp () () =
    Lwt.bind
        (Eliom_sessions.close_session  ~sp:sp ()) 
    (fun () ->
        return (Gs.login_page root_service_with_post sp  "Deconnecté")) ;;

let () =
    Eliom_predefmod.Xhtml.register
        root_service
        root_handler;
    Eliom_predefmod.Xhtml.register
        root_service_with_post
        root_handler_with_post;
    Eliom_predefmod.Xhtml.register
        logout_service
        logout_handler;;
