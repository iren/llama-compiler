{
let num_lines = ref 1

type token =
  | T_eof | T_id | T_int_const | T_string_const | T_char_const 
  | T_and | T_bool | T_char | T_decl | T_def | T_else | T_elsif | T_end
  | T_exit | T_false | T_for | T_head | T_if | T_int | T_list
  | T_mod | T_new | T_nil | T_nil2 | T_not | T_or | T_ref | T_return
  | T_skip | T_tail | T_true | T_plus | T_minus | T_times | T_division
  | T_hash | T_equal | T_inequal | T_less | T_greater
  | T_less_eq | T_greater_eq | T_l_paren | T_r_paren | T_l_br | T_r_br
  | T_comma | T_semicolon | T_colon | T_assign

let error error_string = 
	Printf.eprintf "ERROR,line %d: %s\n"  !num_lines error_string ; exit 1

}

let digit  = ['0'-'9']
let letter = ['A'-'Z''a'-'z']
let white  = [' ' '\t' '\r']
let esc_seq = ("\\n") | "\\t" | "\\r" | "\\0" | "\\\\" | "\\'" | "\\\"" |("\\x"['0'-'9''a'-'f''A'-'F']['0'-'9''a'-'f''A'-'F'])
let common_char = ['\x20'-'\x21'  '\x23'-'\x26' '\x28'-'\x5b' '\x5d'-'\x7e' '\x80'-'\xaf' '\xe0'-'\xf0']

rule lexer = parse
    "and"			{ T_and }
  | "bool"			{ T_bool }
  | "char"			{ T_char }
  | "decl"			{ T_decl }
  | "def"			{ T_def }
  | "else"			{ T_else }
  | "elsif"			{ T_elsif }
  | "end"			{ T_end }
  | "exit"			{ T_exit }
  | "false"			{ T_false }
  | "for"			{ T_for }
  | "head"			{ T_head }
  | "if"			{ T_if }
  | "int"			{ T_int }
  | "list"			{ T_list }
  | "mod"			{ T_mod }
  | "new"			{ T_new }
  | "nil"			{ T_nil }
  | "nil?"			{ T_nil2 }
  | "not"			{ T_not }
  | "or"			{ T_or }
  | "ref"			{ T_ref }
  | "return"		{ T_return }
  | "skip"			{ T_skip }
  | "tail"			{ T_tail }
  | "true"			{ T_true }

  | (letter)(letter|digit|'_'|'?')*					{ T_id }

  | digit+   										{ T_int_const }
  | white+               							{ lexer lexbuf }
  | '\n'											{ incr num_lines; lexer lexbuf  }
  | "%" [^ '\n']* "\n"   							{ incr num_lines; lexer lexbuf  }
  | "<>"											{ T_inequal }
  | "<="											{ T_less_eq }
  | ">="											{ T_greater_eq }
  | '+'												{ T_plus }
  | '-'												{ T_minus }
  | '*'												{ T_times }
  | '/'												{ T_division }
  | '#'												{ T_hash }
  | '='												{ T_equal }
  | '<'												{ T_less }
  | '>'												{ T_greater }
  | ":="											{ T_assign }
  | ':'												{ T_colon } 
  | ';'												{ T_semicolon }
  | ','												{ T_comma }
  | '['												{ T_l_br }
  | ']'												{ T_r_br }
  | '('												{ T_l_paren }
  | ')'										 		{ T_r_paren }
  | "'"(esc_seq|common_char)"'" 					{ T_char_const }
  | '"' (esc_seq|common_char)* '"'  				{ T_string_const }
  | "<*"											{ comments 0 lexbuf }
  |  eof          									{ T_eof }
  |  _		     									{ error "Illegal token" }
  
and comments level = parse
  | "*>"											{ if level = 0 then lexer lexbuf
													  else comments (level-1) lexbuf }
  | "<*"											{ comments (level+1) lexbuf }
  | '\n'											{ incr num_lines; comments level lexbuf }
  | _												{ comments level lexbuf }
  | eof												{ error "Comments are not closed" }

{
  let int_of_token token =
    match token with
      | T_eof    		-> 0
      | T_id 	 		-> 1
      | T_int_const		-> 2
      | T_string_const	-> 3
	  | T_char_const	-> 4
      | T_and			-> 1000	
      | T_bool			-> 1001
      | T_char			-> 1002
      | T_decl			-> 1003
      | T_def			-> 1004
      | T_else			-> 1005
      | T_elsif			-> 1006	
      | T_end			-> 1007
      | T_exit			-> 1008
      | T_false			-> 1009
      | T_for			-> 1010
      | T_head			-> 1011
      | T_if			-> 1012
      | T_int			-> 1013
      | T_list			-> 1014
      | T_mod			-> 1015
      | T_new			-> 1016
      | T_nil			-> 1017
      | T_nil2			-> 1018
      | T_not			-> 1019
      | T_or			-> 1020
      | T_ref			-> 1021
      | T_return		-> 1022
      | T_skip			-> 1023
      | T_tail			-> 1024
      | T_true			-> 1025
	  | T_inequal 		-> 1026
      | T_less_eq	 	-> 1027
 	  | T_greater_eq	-> 1028
	  | T_plus 			-> 1029
      | T_minus			-> 1030
      | T_times			-> 1031
      | T_division		-> 1032
      | T_hash			-> 1033
      | T_equal			-> 1034
      | T_less 			-> 1035
      | T_greater		-> 1036
      | T_assign		-> 1037
      | T_colon 		-> 1038
      | T_semicolon		-> 1039
      | T_comma			-> 1040
      | T_l_br			-> 1041
      | T_r_br			-> 1042
      | T_l_paren		-> 1043
      | T_r_paren		-> 1044

  let main =
    let lexbuf = Lexing.from_channel stdin in
    let rec loop () =
      let token = lexer lexbuf in
      Printf.printf "token=%d, lexeme=\"%s\"\n"
        (int_of_token token) (Lexing.lexeme lexbuf);
      if token <> T_eof then loop () in
    loop ()
}
