%{
open Ast
open Term
open Type
%}

%token EOF
%token <string> IDENTIFIER
// keyword or symbol
%token LET
%token LAM
%token EQ
%token COLON
%token COMMA
// type
%token <string> TYPE
// term
%token CONST

%type <Ast.program> program
%start program

%type <Type.typ> typ

%%

program:
  | top* EOF { $1 }
  ;

top:
  | LET s=IDENTIFIER COLON ty=typ EQ e=expr COMMA
    { Let (s, ty, e) }
  ;

typ:
  | t=TYPE { Ty t }

expr:
  | CONST t=typ { Const t }
  | LAM id=IDENTIFIER t=typ e=expr { Lambda (id, t, e) }
  ;
