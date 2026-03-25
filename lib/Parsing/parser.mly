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

%token INT BOOL
%token PV DP VIR STAR ARR 
%token CONST FUN REC IF AND OR 
%token VAR PROC SET IF2 WHILE CALL (*APS1*)
%token VAR2 ADR (*APS1a*)

%type <Ast.type1> type1
%type <Ast.type1 list> types
%type <Ast.arg> arg
%type <Ast.arg list> args
%type <Ast.argp> argp
%type <Ast.argp list> argsp
%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.exprp> exprp
%type <Ast.exprp list> exprsp
%type <Ast.stat> stat
%type <Ast.def> def
%type <Ast.cmd> cmds
%type <Ast.block> block
%type <Ast.block> prog

%start prog

%%
prog: block    { $1 } 
;

block: LBRA cmds RBRA   { ASTCmds ($2) } 
;

cmds:
  stat                  { ASTStat ($1) }
| def PV cmds           { ASTDef ($1, $3) }
| stat PV cmds          { ASTStats ($1, $3) }
;

def:
  CONST IDENT type1 expr                    { ASTConst ($2, $3, $4) }
| FUN IDENT type1 LBRA args RBRA expr       { ASTFun ($2, $3, $5, $7) }
| FUN REC IDENT type1 LBRA args RBRA expr   { ASTFunRec ($3, $4, $6, $8) }
| VAR IDENT type1                           { ASTVar ($2, $3) }
| PROC IDENT LBRA argsp RBRA block           { ASTProc ($2, $4, $6) }
| PROC REC IDENT LBRA argsp RBRA block       { ASTProcRec ($3, $5, $7) }
;

type1:
  INT               { ASTInt }
| BOOL              { ASTBool }
| LPAR types ARR type1 RPAR     { ASTTypeT($2, $4) }
;

types:
    type1              { [$1] }
  | type1 STAR types   { $1::$3 }
;

arg:
  IDENT DP type1  { ASTArg($1, $3) }
;

args :
  arg            { [$1] } 
| arg VIR args   { $1::$3 }
;

argp:
  IDENT DP type1  { ASTArgp($1, $3) }
| VAR2 IDENT DP type1  { ASTArgVar($2, $4) }
;

argsp:
  argp            { [$1] } 
| argp VIR argsp   { $1::$3 }
;

stat:
  ECHO expr             { ASTEcho($2) }
| SET IDENT expr        { ASTSet ($2, $3) }
| IF2 expr block block  { ASTIf2 ($2, $3, $4) }
| WHILE expr block      { ASTWhile ($2, $3) }
| CALL IDENT exprsp     { ASTCall ($2, $3) }
;

exprp:
  expr                  { ASTVal($1) }
| LPAR ADR IDENT RPAR   { ASTRef($3) }
;

exprsp:
  exprp       { [$1] }
| exprp exprsp { $1::$2 }
;

expr:
  NUM                           { ASTNum($1) }
| IDENT                         { ASTId($1) }
| LPAR IF expr expr expr RPAR   { ASTIf($3, $4, $5) }
| LPAR AND expr expr RPAR       { ASTAnd($3, $4) }
| LPAR OR expr expr RPAR        { ASTOr($3, $4) }
| LBRA args RBRA expr           { ASTAbs($2, $4) }
| LPAR expr exprs RPAR          { ASTApp($2, $3) }
;

exprs:
  expr       { [$1] }
| expr exprs { $1::$2 }
;

