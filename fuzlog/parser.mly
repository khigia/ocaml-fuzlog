%{
open Ast

%}

%token IF THEN
%token IS
%token AND
%token SEP
%token DEF
%token OPENB CLOSEB
%token OPENSB CLOSESB
%token EOF
%token <string> SYMB
%token <string> NUMBER

%start rules vocabulary
%type <Ast.rule_t list> rules
%type <Ast.def_t list> vocabulary

%%

/* TODO parse a vocabulary section */

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

vocabulary:
    vocabulary_
        { List.rev $1 }
;

vocabulary_:
    def
        { [ $1 ] }
    | vocabulary_ def
        { $2 :: $1 }
;

def:
    DEF symb vocfun numbers
        { DefByFun($2, $3, $4) }
    | DEF symb OPENSB points CLOSESB
        { DefByPoints($2, $4) }
;

vocfun:
    symb
        { $1 }
;

points:
    points_
        { List.rev $1 }
;

points_:
    point
        { [ $1 ] }
    | points_ point
        { $2 :: $1 }
;

point:
    OPENB number number CLOSEB
        { ($2, $3) }
;

numbers:
    numbers_
        { List.rev $1 }
;

numbers_:
    number
        { [ $1 ] }
    | numbers_ number
        { $2 :: $1 }
;

number:
    NUMBER
        { float_of_string $1 }
;

symb:
    SYMB
        { Symb $1 }
;

%%

