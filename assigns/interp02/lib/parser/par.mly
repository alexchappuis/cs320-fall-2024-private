%{
open Utils

let rec mk_app e es =
  match es with
  | [] -> e
  | x :: es -> mk_app (App (e, x)) es
%}

%token <int> NUM
%token <string> VAR
%token EOF
%token IF THEN ELSE LET REC IN FUN ARROW
%token ADD SUB MUL DIV MOD LT LTE GT GTE EQ NEQ AND OR
%token LPAREN RPAREN COLON
%token TRUE FALSE UNIT
%token INTTY BOOLTY UNITTY
%token ASSERT

%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.prog> prog

%%

prog:
  | tops=toplet* EOF { tops }

toplet:
  | LET; x=VAR; args=args_opt; COLON; ty=ty; EQ; e=expr
    { TopLet (x, args, ty, e) }
  | LET; REC; x=VAR; args=args; COLON; ty=ty; EQ; e=expr
    { TopLetRec (x, args, ty, e) }

args_opt:
  | args=args { args }
  | { [] }

args:
  | arg=args1 args=args1* { arg :: args }
  | { [] }

args1:
  | LPAREN; x=VAR; COLON; ty=ty; RPAREN { (x, ty) }

ty:
  | INTTY { IntTy }
  | BOOLTY { BoolTy }
  | UNITTY { UnitTy }
  | t1=ty; ARROW; t2=ty { FunTy (t1, t2) }
  | LPAREN; t=ty; RPAREN { t }

expr:
  | LET; x=VAR; args=args_opt; COLON; ty=ty; EQ; e1=expr; IN; e2=expr
    { Let (x, args, ty, e1, e2) }
  | LET; REC; x=VAR; args=args; COLON; ty=ty; EQ; e1=expr; IN; e2=expr
    { LetRec (x, args, ty, e1, e2) }
  | IF; e1=expr; THEN; e2=expr; ELSE; e3=expr
    { If (e1, e2, e3) }
  | FUN; args=args; ARROW; e=expr
    { Fun (args, e) }
  | e=expr2 { e }

expr2:
  | e1=expr2; op=bop; e2=expr2 { BinOp (op, e1, e2) }
  | ASSERT; e=expr3 { Assert e }
  | e=expr3; es=expr3* { mk_app e es }

expr3:
  | UNIT { Unit }
  | TRUE { True }
  | FALSE { False }
  | n=NUM { Num n }
  | x=VAR { Var x }
  | LPAREN; e=expr; RPAREN { e }

bop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQ { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }
