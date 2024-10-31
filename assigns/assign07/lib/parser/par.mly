%{
open Utils
%}

%token <int> NUM
%token <string> VAR
%token EOF
%token IF 
%token THEN
%token ELSE 
%token LET 
%token IN
%token FUN
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token ARROW
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token LT
%token LTE
%token GT
%token GTE
%token EQ
%token NEQ
%token AND
%token OR


%left OR
%left AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD
%nonassoc THEN ELSE

%start <Utils.prog> prog

%%

prog:
  | EOF { Num 0 }

expr:
  | IF expr THEN expr ELSE expr { If ($2, $4, $6) }
  | LET VAR EQUALS expr IN expr { Let ($2, $4, $6) }
  | FUN VAR ARROW expr { Fun ($2, $4) }
  | expr2 { $1 }

expr2:
  | expr2 bop expr2 { Bop ($2, $1, $3) }
  | expr3 expr3s { mk_app $1 $2 }

expr3:
  | LPAREN expr RPAREN { $2 }
  | NUM { Num $1 }
  | VAR { Var $1 }
  | TRUE { True }
  | FALSE { False }
  | LPAREN RPAREN { Unit }

expr3s:
  | { [] }
  | expr3 expr3s { $1 :: $2 }

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
