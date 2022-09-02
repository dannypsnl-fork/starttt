%{
open Ast
%}

%token EOF
%token <int> INT_CONSTANT
%token <float> FLOAT_CONSTANT
%token <string> WORD
%token LET
%token LAM
%token EQ

%type <Ast.program> program
%start program

%%

program:
  | top* EOF { Program $1 }
  ;

top:
  | LET s=WORD EQ LAM e=expr { Let (s, e) }

expr:
  | i=INT_CONSTANT { Int i }
  | f=FLOAT_CONSTANT { Float f }
  | s=WORD { Word s }
  ;
