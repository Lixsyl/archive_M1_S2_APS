%{
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}
  
%token <int> NUM
%token <string> IDENT
%token LPAR RPAR 
%token LBRA RBRA
%token ECHO

%token <int> INT
%token <bool> BOOL
%token PV DP VIR STAR ARR 
%token CONST FUN REC IF AND OR 

// QUESTION
%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmd list> cmds
%type <Ast.cmd list> prog

%start prog

%%
prog: LBRA cmds RBRA    { $2 } // QUESTION
;

cmds:
  stat                  { [ASTStat $1] }
| def PV cmds           { ASTDef ($1, $3)}
;

def:
  CONST IDENT type1 expr                { ASTConst($2, $3, $4) }
| FUN IDENT type1 LBRA args RBRA expr   { ASTFun($2, $3, $5, $7) }
| FUN REC IDENT type1 LBRA args RBRA expr   { ASTFun($3, $4, $6, $8) }
;

type1:
  INT               { ASTInt($1) }
| BOOL              { ASTBool($1) }
| types ARR type1   { ASTTypeT($1, $3) }
;

types : // QUESTION
  type1         { [$1] } 
| type1 STAR types   { $1::$2 }
;

arg:
  IDENT DP type1  { ASTArg($1, $3) }
;

args : // QUESTION
  arg         { [$1] } 
| arg VIR args   { $1::$2 }
;

stat:
  ECHO expr             { ASTEcho($2) }
;

expr:
  NUM                   { ASTNum($1) }
| IDENT                 { ASTId($1) }
| LPAR IF expr expr expr RPAR  { ASTIf($3, $4, $5) }
| LPAR AND expr expr RPAR      { ASTAnd($3, $4) }
| LPAR OR expr expr RPAR       { ASTOr($3, $4) }
| LBRA args RBRA expr   { ASTAno($2, $4) }
| LPAR expr exprs RPAR  { ASTApp($2, $3) }
;

exprs : // QUESTION
  expr       { [$1] }
| expr exprs { $1::$2 }
;

