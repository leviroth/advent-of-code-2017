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
let digits = digit+
let letter = ['a' - 'z']
let letters = letter+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
rule read =
  parse
  | white           { read lexbuf }
  | newline         { next_line lexbuf; read lexbuf }
  | "->"            { ARROW }
  | ","             { COMMA }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | (letters as s)  { NAME s }
  | (digits as d)   { WEIGHT (int_of_string d)}
  | eof             { EOF }
