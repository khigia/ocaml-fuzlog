%{
open Ast
%}

%token IF THEN
%token IS
%token AND
%token SEP
%token EOF
%token <string> SYMB

%start rules
%type <Ast.rule_t list> rules

%%

rules:
    rule
        { [ $1 ] }
    | rules rule
        { $2 :: $1 }

rule:
    IF is_branch THEN is_branch
        { Imply($2, $4) }
;

is_branch:
    is
        { $1 }
    | is_branch AND is
        { And($1, $3) }
;

is:
    SYMB IS SYMB
        { Is(Symb($1), Symb($3)) }
;
