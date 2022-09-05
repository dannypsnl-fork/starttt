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
%token TYPE_MARK 
// term
%token CONST_MARK

%type <Ast.program> program
%start program

%type <Type.typ> typ

%%

program:
  | top* EOF { $1 }
  ;

top:
  | LET s=IDENTIFIER COLON ty=marked_typ EQ e=expr COMMA
    { Let (s, ty, e) }
  ;

marked_typ: TYPE_MARK t=typ { t }
typ:
  | t=IDENTIFIER { Ty t }

expr:
  | CONST_MARK t=typ { Const t }
  | LAM id=IDENTIFIER t=typ e=expr { Lambda (id, t, e) }
  ;
