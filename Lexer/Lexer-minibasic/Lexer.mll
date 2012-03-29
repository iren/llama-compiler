{
type token =
  | T_eof | T_const | T_var 
  | T_print | T_let | T_for | T_do | T_begin | T_end | T_if | T_then
  | T_eq | T_lparen | T_rparen | T_plus | T_minus | T_times
}

let digit  = ['0'-'9']
let letter = ['a'-'z']
let white  = [' ' '\t' '\r' '\n']

rule lexer = parse
    "print"  { T_print }
  | "let"    { T_let }
  | "for"    { T_for }
  | "do"     { T_do }
  | "begin"  { T_begin }
  | "end"    { T_end }
  | "if"     { T_if }
  | "then"   { T_then }

  | digit+   { T_const }
  | letter   { T_var }

  | '='      { T_eq }
  | '('      { T_lparen }
  | ')'      { T_rparen }
  | '+'      { T_plus }
  | '-'      { T_minus }
  | '*'      { T_times }

  | white+               { lexer lexbuf }
  | "'" [^ '\n']* "\n"   { lexer lexbuf }

  |  eof          { T_eof }
  |  _ as chr     { Printf.eprintf "invalid character: '%c' (ascii: %d)"
                      chr (Char.code chr);
                    lexer lexbuf }

{
  let string_of_token token =
    match token with
      | T_eof    -> "T_eof"
      | T_const  -> "T_const"
      | T_var    -> "T_var"
      | T_print  -> "T_print"
      | T_let    -> "T_let"
      | T_for    -> "T_for"
      | T_do     -> "T_do"
      | T_begin  -> "T_begin"
      | T_end    -> "T_end"
      | T_if     -> "T_if"
      | T_then   -> "T_then"
      | T_eq     -> "T_eq"
      | T_lparen -> "T_lparen"
      | T_rparen -> "T_rparen"
      | T_plus   -> "T_plus"
      | T_minus  -> "T_minus"
      | T_times  -> "T_times"

  let main =
    let lexbuf = Lexing.from_channel stdin in
    let rec loop () =
      let token = lexer lexbuf in
      Printf.printf "token=%s, lexeme=\"%s\"\n"
        (string_of_token token) (Lexing.lexeme lexbuf);
      if token <> T_eof then loop () in
    loop ()
}
