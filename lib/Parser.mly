%{
open Ast
%}

%token EOF
%token <int> INT_CONSTANT
%token <float> FLOAT_CONSTANT
%token <string> IDENTIFIER
%token <string> STRING_LITERAL
// keyword or symbol
%token LET
%token LAM
%token EQ
%token COLON
%token COMMA
%token QUOTE
// type
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
  | LET s=IDENTIFIER COLON ty=typ EQ e=expr COMMA
    { Let (s, ty, e) }
  ;

typ:
  | INT { TyInt }
  | FLOAT { TyFloat }
  | STRING { TyString }

expr:
  | i=INT_CONSTANT { Int i }
  | f=FLOAT_CONSTANT { Float f }
  | s=STRING_LITERAL { String s }
  | LAM id=IDENTIFIER e=expr { Lambda (id, e) }
  ;
