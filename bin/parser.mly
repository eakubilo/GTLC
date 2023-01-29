%{
open AST
%}

%token <int> INT
%token <string> ID
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token UNTYPEDLAM
%token LAM
%token <string>LABEL
%token EOF
%token APP

%token INT_TYPE
%token BOOL_TYPE
%token ARROW_TYPE
%token DYN_TYPE

%start <AST.expr> prog

%%

prog:
    | e = expr; EOF { e }
    ;

expr:
    | LAM; x = ID; t = types ; e = expr {Lam (Var(x), t, e)}
    | i = INT { Int i}
    | x = ID { Var x }
    | TRUE { True }
    | FALSE { False }
    | LAM; x=ID; e= expr {Lam(Var(x), Dyn, e)}
    | LPAREN; e1 = expr; e2 = expr; RPAREN; l = LABEL {App(e1, e2, Label(l))}
    | LPAREN; e=expr; RPAREN {e} 

types:
    | INT_TYPE {B(Int)}
    | BOOL_TYPE {B(Bool)}
    | t1 = types; ARROW_TYPE; t2 = types { Arrow(t1, t2)}
    | DYN_TYPE {Dyn}
    | LPAREN; t=types; RPAREN {t} 

;