{
open Parser
open Lexing

let incr_lineno lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

let alphanum = ['_''a'-'z''A'-'Z''0'-'9']
let digit = ['0'-'9']

rule rule_tokens = parse
  | [' ' '\t']      { rule_tokens lexbuf }
  | ['\n']          { incr_lineno lexbuf; rule_tokens lexbuf }
  | "IF"|"if"       { IF }
  | "THEN"|"then"   { THEN }
  | "IS"|"is"       { IS }
  | "AND"|"and"     { AND }
  | alphanum+ as s  { SYMB(s) }
  | eof             { EOF }

and voc_tokens = parse
  | [' ' '\t']      { voc_tokens lexbuf }
  | ['\n']          { incr_lineno lexbuf; voc_tokens lexbuf }
  | "DEF"|"def"     { DEF }
  | "["             { OPENSB }
  | "]"             { CLOSESB }
  | "("             { OPENB }
  | ")"             { CLOSEB }
  | "-"? digit+
  | "-"? digit* "." digit+
  | "-"? digit+ "." digit* as num
                    { NUMBER(num) }
  | alphanum+ as s  { SYMB(s) }
  | eof             { EOF }
