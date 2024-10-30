%{
open Utils
%}

%token LET
%token EQUALS
%token IN
%token ADD
%token SUB
%token MUL
%token DIV
%token <int> NUM
%token LPAREN
%token RPAREN

%token EOF

%start <Utils.prog> prog

%%

prog:
  | expr; EOF { e }

expr:
  | LET; x = var; EQUALS; e1 = expr; IN; e2 = expr
    {LET (x, e1, e2)}
  | e = expr1 { e }

$inline bop:
  | ADD { Add}
  | SUB { Sub}
  | MUL { Mul}
  | DIV { Div}

expr1 : 
  | e1 = expr1; op = bop; e2 = expr1 {Bop (op, e1, e2)}
  | num {}
  | x = VAR {var x}
  | LPAREN; e = expr; RPAREN {e}

