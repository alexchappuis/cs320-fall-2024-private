%{
open Utils

let rec mk_sapp e es =
  match es with
  | [] -> e
  | x :: es -> mk_sapp (SApp (e, x)) es
%}

%token <int> NUM
%token <string> VAR
%token EOF
%token IF THEN ELSE LET IN FUN ARROW
%token ADD SUB MUL DIV MOD LT LTE GT GTE EQ NEQ AND OR EQUALS
%token LPAREN RPAREN
%token TRUE FALSE UNIT
%token INTTY BOOLTY UNITTY
%token REC

%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD
%left EQUALS 

%start <Utils.toplet list> prog

%%

prog:
  | toplets=toplet_list EOF { toplets }

toplet_list:
  | t=toplet { [t] }
  | ts=toplet_list t=toplet { t :: ts }

toplet:
  | LET; is_rec=rec_flag; name=VAR; args=args_opt; COLON; ty=ty; EQ; value=sfexpr {
      { is_rec; name; args; ty; value; }
    }


rec_flag:
  | REC { true }
  | { false }

sfexpr:
  | IF; e1=sfexpr; THEN; e2=sfexpr; ELSE; e3=sfexpr { SIf (e1, e2, e3) }
  | LET; is_rec=rec_flag; name=VAR; args=args_opt; COLON; ty=ty; EQ; value=sfexpr; IN; body=sfexpr {
      SLet { is_rec; name; args; ty; value; body }
    }
  | FUN; arg=arg; args=args_opt; ARROW; body=sfexpr {
      SFun { arg; args; body }
    }
  | ASSERT; e=sfexpr { SAssert e }
  | e=sfexpr2 { e }

sfexpr2:
  | e1=sfexpr2; op=sbop; e2=sfexpr2 { SBop (op, e1, e2) }
  | e=sfexpr3; es=sfexpr3* { mk_sapp e es }

sfexpr3:
  | UNIT { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
  | n=NUM { SNum n }
  | x=VAR { SVar x }
  | LPAREN; e=sfexpr; RPAREN { e }

%inline sbop:
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

arg:
  | v=VAR; COLON; ty=ty { (v, ty) }

args_opt:
  | { [] }
  | args=args { args }

args:
  | LPAREN; arg=arg; RPAREN { [arg] }
  | args=args; LPAREN; arg=arg; RPAREN { arg :: args }

ty:
  | INTTY { IntTy }
  | BOOLTY { BoolTy }
  | UNITTY { UnitTy }
  | t1=ty; ARROW; t2=ty { FunTy (t1, t2) }
  | LPAREN; t=ty; RPAREN { t }
