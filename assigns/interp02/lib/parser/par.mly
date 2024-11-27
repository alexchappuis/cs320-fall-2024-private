%{
open Utils
%}

%token EOF
%token FUN "fun"
%token ARROW "->"
%token LPAREN "("
%token RPAREN ")"
%token LET "let"
%token REC "rec"
%token EQ "="
%token IN "in"
%token <string> VAR
%token <int> NUM
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token ADD "+"
%token MUL "*"
%token DIV "/"
%token SUB "-"
%token TRUE "true"
%token FALSE "false"
%token ASSERT "assert"
%token COLON ":"
%token INTTY "int"
%token BOOLTY "bool"
%token UNIT "()"
%token UNITTY "unit"
%token MOD "mod"
%token AND "&&"
%token OR "||"
%token LT "<"
%token LTE "<="
%token GT ">"
%token GTE ">="
%token NEQ "<>"


%right ARROW
%left OR
%left AND
%left LT LTE GT GTE EQUALS NEQ
%left PLUS MINUS
%left TIMES MOD
%start <Utils.prog> prog

%%

prog:
  | ls = toplet* EOF { ls }

toplet:
  | LET x = VAR args = arg* COLON ty = ty EQ e = expr
    { { is_rec = false; name = x; args = args; ty; value = e } }
  | LET REC f = VAR args = arg* COLON ty = ty EQ e = expr
   { { is_rec = false; name = f; args = args; ty; value = e } }

arg:
  | LPAREN x = VAR COLON ty = ty RPAREN { (x, ty) }

ty:
  | INTTY { IntTy }
  | BOOLTY { BoolTy }
  | UNITTY { UnitTy }
  | t1 = ty ARROW t2 = ty { FunTy (t1, t2) }
  | LPAREN ty = ty RPAREN { ty }

expr:
  | LET x = VAR args = arg* COLON ty = ty EQ e1 = expr IN e2 = expr
   { SLet { is_rec = false; name = x; args = args; ty; value = e1; body = e2 } }
  | LET REC x = VAR arg = arg ; args = arg* COLON ty = ty EQ e1 = expr IN e2 = expr
   { SLet { is_rec = true; name = x; args = arg :: args; ty; value = e1; body = e2 } }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr
   { SIf (e1, e2, e3) }
  | FUN arg = arg ; args = arg* ARROW e = expr
  { SFun { arg = arg; args = args; body = e } }
  | e = expr2 { e }

expr2:
  | e1 = expr2 bop = bop e2 = expr2 { SBop (bop, e1, e2) }
  | ASSERT e = expr3 { SAssert e }
  | e = expr3 { e }

expr3:
  | UNIT { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
  | n = NUM { SNum n }
  | x = VAR { SVar x }
  | LPAREN e = expr RPAREN { e }

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
