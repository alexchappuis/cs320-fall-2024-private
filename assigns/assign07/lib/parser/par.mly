%{
open Utils
%}

%token <int> NUM
%token <string> VAR
%token IF THEN ELSE
%token LET IN
%token FUN ARROW  
%token TRUE FALSE UNIT
%token LPAREN RPAREN

%token ADD SUB MUL DIV MOD
%token LT LTE GT GTE EQ NEQ
%token AND OR

%token EOF

%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.prog> prog

%%


prog:
  | e = expr; EOF { e }

expr:
  | LET; x = VAR; EQ; e1 = expr; IN; e2 = expr
    { Let (x, e1, e2) }
  | FUN; x = VAR; ARROW; body = expr { Fun (x, body) }
  | IF; cond = expr; THEN; t_expr = expr; ELSE; e_expr = expr { If (cond, t_expr, e_expr) }
  | e = expr1 { e }

%inline bop:
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

expr1:
  | e1 = expr1; op = bop; e2 = expr2 { Bop (op, e1, e2) }
  | e = expr2 { e }

expr2:
  | e = expr3; es = expr3 { App (e, es) }
  | e = expr3 { e }

expr3:
  | UNIT { Unit }
  | TRUE { True }
  | FALSE { False }
  | n = NUM { Num n }
  | x = VAR { Var x }
  | LPAREN; e = expr; RPAREN { e }