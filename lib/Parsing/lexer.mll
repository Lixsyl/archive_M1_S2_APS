(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* ==  Lexique                                                             == *)
(* ========================================================================== *)

{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

}
rule token = parse
    [' ' '\t' '\n']       { token lexbuf }     (* skip blanks *)
  | '['              { LBRA }
  | ']'              { RBRA }
  | '('              { LPAR }
  | ')'              { RPAR }
  | ';'              { PV   }
  | ':'              { DP   }
  | ','              { VIR  }
  | '*'              { STAR }
  | "->"             { ARR  }
  | "CONST"          { CONST }
  | "FUN"            { FUN  }
  | "REC"            { REC  }
  | "ECHO"           { ECHO }
  | "if"             { IF   }
  | "and"            { AND  }
  | "or"             { OR   }
  | "bool"           { BOOL }
  | "int"            { INT  }
  | "VAR"            { VAR  } (*APS1*)
  | "PROC"           { PROC } (*APS1*)
  | "SET"            { SET  } (*APS1*)
  | "IF"             { IF2   } (*APS1*)
  | "WHILE"          { WHILE } (*APS1*)
  | "CALL"           { CALL } (*APS1*)
  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof              { raise Eof }
