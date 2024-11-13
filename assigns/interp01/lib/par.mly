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
%token IF
%token THEN
%token ELSE
%token IN
%token FUN
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
%token ARROW
%token LET
%token RPAREN
%token LPAREN
%token TRUE
%token FALSE
%token LCURLY
%token RCURLY
%token UNIT

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
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If(e1, e2, e3) }
  | LET; x = VAR; EQ; e1 = expr; IN; e2 = expr { Let(x, e1, e2) }
  | FUN; x = VAR; ARROW; e1 = expr { Fun(x, e1) }
  | e = expr2 {e}

expr2:
  | e1 = expr2; op = bop; e2 = expr2 { Bop(op, e1, e2) }
  | e = expr3; es = expr3* { mk_app e es }


expr3:
  | TRUE { True }
  | FALSE { False }
  | UNIT { Unit }
  | n = NUM { Num n }
  | x = VAR { Var x }
  | LPAREN; e = expr; RPAREN { e } 

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