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
    rules_ { List.rev $1 }

rules_:
    rule
        { [ $1 ] }
    | rules_ rule
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
    symb IS symb
        { Is($1, $3) }
;

symb:
    SYMB
        { Symb $1 }
;
