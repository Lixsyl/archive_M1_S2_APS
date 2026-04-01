open Aps_syntax
(* open PrologTerm *)
open PrologTerm
open Manip_sys
open Aps_syntax.Interpreter


let print_prog () =
  let fname = Sys.argv.(1) in
  let p = get_prog fname in

    pp_prog Format.str_formatter p;
    let s = Format.flush_str_formatter () in
    Format.printf "==== Test du pretty printer de termes ====\n %s " s ;
    Format.printf "==== Test du typage du programme ====\n" ;
    match cmd_typ  s with
    | Ok(s,_) -> Format.printf "%s\n" s ; 
                  if s = "OK" then let p = get_prog fname in Format.printf "%s |\t Résultat : %s\n" fname (print_flux (let (_, res) = i_prog p in res)) ;
    | Error (`Msg m) -> print_endline m 


let _ = print_prog ()