%{
open Utils

let rec mk_app e es =
  match es with
  | [] -> e
  | x :: es -> mk_app (App (e, x)) es
%}

%token EOF
%token FUN "fun"
%token ARROW "->"
%token LPAREN "("
%token RPAREN ")"
%token LET "let"
%token REC "rec"
%token EQUALS "="
%token IN "in"
%token <string> VAR
%token <int> NUM
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token PLUS "+"
%token TIMES "*"
%token MINUS "-"
%token TRUE "true"
%token FALSE "false"
%token COLON ":"
%token INTTY "int"
%token BOOLTY "bool"
%token UNIT "()"
%token UNITTY "unit"

%right ARROW
%left EQUALS
%left PLUS MINUS
%left TIMES

%start <Ast.prog> prog

%%

prog:
  | ls=toplet* EOF { ls }

toplet:
  | LET; x=VAR; COLON; ty=ty; EQUALS; e=expr { TopLet(x, ty, e) }
  | LET; REC; f=VAR; LPAREN; x=VAR; COLON; ty_arg=ty; RPAREN;
    COLON; ty_out=ty; EQUALS; e=expr { TopLetRec(f, x, ty_arg, ty_out, e) }

ty:
  | INTTY { IntTy }
  | BOOLTY { BoolTy }
  | UNITTY { UnitTy }
  | t1=ty; ARROW; t2=ty { FunTy(t1, t2) }
  | LPAREN; t=ty; RPAREN { t }

expr:
  | LET; x=VAR; COLON; ty=ty; EQUALS; e1=expr; IN; e2=expr { Let(x, ty, e1, e2) }
  | LET; REC; f=VAR; LPAREN; x=VAR; COLON; ty_arg=ty; RPAREN;
    COLON; ty_out=ty; EQUALS; e1=expr; IN; e2=expr { LetRec(f, x, ty_arg, ty_out, e1, e2) }
  | IF; e1=expr; THEN; e2=expr; ELSE; e3=expr { If(e1, e2, e3) }
  | FUN; LPAREN; x=VAR; COLON; ty=ty; RPAREN; ARROW; e=expr { Fun(x, ty, e) }
  | e=expr2 { e }

expr2:
  | e1=expr2; PLUS; e2=expr2 { Add(e1, e2) }
  | e1=expr2; TIMES; e2=expr2 { Mul(e1, e2) }
  | e1=expr2; MINUS; e2=expr2 { Sub(e1, e2) }
  | e1=expr2; EQUALS; e2=expr2 { Eq(e1, e2) }
  | e=expr3; es=expr3* { mk_app e es }

expr3:
  | VAR { Var($1) }
  | NUM { Num($1) }
  | UNIT { Unit }
  | TRUE { True }
  | FALSE { False }
  | LPAREN; e=expr; RPAREN { e }
