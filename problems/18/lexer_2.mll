{
open Lexing
open Parser_2

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

let digit = ['0' - '9']
let digits = '-'? digit+
let alpha = ['a' - 'z']
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
rule read =
  parse
  | white           { read lexbuf }
  | newline         { next_line lexbuf; read lexbuf }
  | "snd"           { SND }
  | "set"           { SET }
  | "add"           { ADD }
  | "mul"           { MUL }
  | "mod"           { MOD }
  | "rcv"           { RCV }
  | "jgz"           { JGZ }
  | (digits as d)   { NUMBER (int_of_string d)}
  | (alpha as a)    { REGISTER (a) }
  | eof             { EOF }
