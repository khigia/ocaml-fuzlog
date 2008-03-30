{
open Parser
}

let alphanum = ['_''a'-'z''A'-'Z''0'-'9']

rule tokens = parse
  | [' ' '\n' '\t'] { tokens lexbuf }
  | "IF"|"if"       { IF }
  | "THEN"|"then"   { THEN }
  | "IS"|"is"       { IS }
  | "AND"|"and"     { AND }
  | alphanum+ as s  { SYMB(s) }
  | eof             { EOF }
