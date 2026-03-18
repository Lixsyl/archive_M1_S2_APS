open Aps_syntax.Manip_sys
open Aps_syntax.PrologTerm
open Aps_syntax.Interpreter

let l_test_0 = [(testfile_name 0 0, "OK"); 
                (testfile_name 0 1, "KO"); 
                (testfile_name 0 2, "KO");
                (testfile_name 0 3, "KO");
                (testfile_name 0 4, "OK");
                (testfile_name 0 5, "OK");
                (testfile_name 0 6, "OK")]

let l_test_1 = [(testfile_name 1 7, "OK");
                (testfile_name 1 8, "OK");
                (testfile_name 1 9, "OK");
                (testfile_name 1 10, "OK")]


let test_prologTerm (l_test : string list) =
List.fold_right
(fun fname _ ->
  let p = get_prog fname in
      Format.printf "%s |\t %a\n" fname pp_prog p ;

) l_test ()

let test_typeur (l_test : (string * string) list ) =
List.fold_right
(fun (fname,expected) _ ->
  let p = get_prog fname  in
      pp_prog Format.str_formatter p;
      let s = Format.flush_str_formatter () in
      match cmd_typ s with
      | Ok(s,_) ->  (Format.printf "%s |\t Résultat du typeur : %s\t Résultat attendu : %s\n" fname s expected); 
                    if s = "OK" then let p = get_prog fname in Format.printf "%s |\t Résultat : %s\n" fname (print_flux (let (_, res) = i_prog p in res)) ;
      | Error (`Msg m) -> print_endline m

) l_test ()

let _ =
  Format.printf "========== Tests de APS 0 ==========\n";
  Format.printf "- Test de PrologTerm\n";
  test_prologTerm (fst (List.split l_test_0 ));
  print_endline "- Test du typeur\n";
  test_typeur l_test_0;
  Format.printf "========== Tests de APS 1 ==========\n";
  Format.printf "- Test de PrologTerm\n";
  test_prologTerm (fst (List.split l_test_1 ));
  print_endline "- Test du typeur\n";
  test_typeur l_test_1;

