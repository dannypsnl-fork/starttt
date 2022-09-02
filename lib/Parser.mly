%{
open Ast
%}

%token EOF
%token <int> INT_CONSTANT
%token <float> FLOAT_CONSTANT
%token <string> WORD

%type <Ast.program> program
%start program

%%

program:
  | atom* EOF { Program $1 }
  ;

atom:
  | i=INT_CONSTANT { Int i }
  | f=FLOAT_CONSTANT { Float f }
  | s=WORD { Word s }
  ;
