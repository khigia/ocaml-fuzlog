{
open Parser
}

rule tokens = parse
  | [' ' '\n' '\t'] { tokens lexbuf }
  | "IF"|"if"       { IF }
  | "THEN"|"then"   { THEN }
  | "IS"|"is"       { IS }
  | "AND"|"and"     { AND }
  | ['a'-'z']+ as s { SYMB(s) }
  | eof             { EOF }
