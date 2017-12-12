{
open Lexing
open Parser

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

let digit = ['0' - '9']
let digits = '-'? digit+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
rule read =
  parse
  | white           { read lexbuf }
  | newline         { next_line lexbuf; read lexbuf }
  | " <-> "            { PIPE }
  | ","           { COMMA }
  | (digits as d)   { NUMBER (int_of_string d)}
  | eof             { EOF }
