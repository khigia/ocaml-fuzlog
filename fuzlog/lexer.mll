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

rule tokens = parse
  | [' ' '\t']      { tokens lexbuf }
  | ['\n']          { incr_lineno lexbuf; tokens lexbuf }
  | "IF"|"if"       { IF }
  | "THEN"|"then"   { THEN }
  | "IS"|"is"       { IS }
  | "AND"|"and"     { AND }
  | alphanum+ as s  { SYMB(s) }
  | eof             { EOF }
