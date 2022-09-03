%{
open Ast
%}

%token EOF
%token <int> INT_CONSTANT
%token <float> FLOAT_CONSTANT
%token <string> STRING_LITERAL
%token LET
%token LAM
%token EQ
%token INT
%token FLOAT
%token STRING

%type <Ast.program> program
%start program

%type <Ast.typ> typ

%%

program:
  | top* EOF { $1 }
  ;

top:
  | LET s=STRING_LITERAL ty=typ EQ LAM e=expr { Let (s, ty, e) }
  ;

typ:
  | INT { Int }
  | FLOAT { Float }
  | STRING { String }

expr:
  | i=INT_CONSTANT { Int i }
  | f=FLOAT_CONSTANT { Float f }
  | s=STRING_LITERAL { Word s }
  ;
