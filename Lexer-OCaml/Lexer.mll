{
    let ln_cnt = ref 1
    
    type token =
        | T_Andfun      
        | T_Let         
        | T_In          
        | T_Rec         
        | T_Array       
        | T_Dim         
        | T_Begin       
        | T_End         
        | T_Delete      
        | T_New         
        | T_Do          
        | T_Done        
        | T_Downto      
        | T_For         
        | T_To          
        | T_While       
        | T_If          
        | T_Then        
        | T_Else        
        | T_Match       
        | T_With        
        | T_Mutable     
        | T_Ref         
        | T_Of          
        | T_Type        
        | T_Tbool       
        | T_Tchar       
        | T_Tfloat      
        | T_Tint        
        | T_Tunit       
        | T_Minus       
        | T_Plus        
        | T_Times       
        | T_Divide      
        | T_Mod         
        | T_Flminus     
        | T_Flplus      
        | T_Fltimes     
        | T_Fldivide    
        | T_Flpower     
        | T_Eq          
        | T_Ge          
        | T_Gt          
        | T_Le          
        | T_Lt          
        | T_Neq         
        | T_Nateq       
        | T_Natneq      
        | T_And         
        | T_Or          
        | T_Not         
        | T_Assign      
        | T_Bang        
        | T_Semicolon   
        | T_Pipe      
        | T_Gives       
        | T_Lbrack      
        | T_Rbrack      
        | T_Lparen      
        | T_Rparen      
        | T_Colon       
        | T_Comma       
        | T_Eof         
        | T_Bool        
        | T_True
        | T_False
        | T_Char        
        | T_Float       
        | T_Int         
        | T_Unit        
        | T_Conid       
        | T_Genid       
        | T_Cstring     

    let error error_string = 
	    Printf.eprintf "ERROR,line %d: %s\n"  !ln_cnt error_string ; exit 1
    
    let fileP () = if Array.length Sys.argv > 0
        then Sys.argv.(1)
        else "<stdin>"
}

let digit    = ['0'-'9']
let intnum   = ['0'-'9']+
let floatnum = intnum"."intnum(('e'|'E')(('+'|'-')?)intnum)?
(*let hexdigit = ['0'-'9''a'-'f''A'-'F']
let hexpair  = (hexdigit)(hexdigit)*)
               
let lowChar = ['a'-'z']
let uppChar = ['A'-'Z']
let idChar  = ['a'-'z''A'-'Z''0'-'9''_']
let symChar = [' ' '!' '#' '$' '%' '^' '&' '*' '(' ')' '-' '=' '+' ':' ';' '[' '{' ']' '}' '|' ',' '<' '.' '>' '/' '?' '`' '~' '@']
let oneChar = symChar | idChar
let squote   = '\''
               
let genid    = lowChar idChar*
let conid    = uppChar idChar*

let white    = [' ''\t']+
let eol      = ['\r''\n']|"\r\n"
let esc_seq  = ("\\n") | "\\t" | "\\r" | "\\0" | "\\\\" | "\\'" | "\\\"" |("\\x"['0'-'9''a'-'f''A'-'F']['0'-'9''a'-'f''A'-'F'])
let cmn_char = ['\x20'-'\x21'  '\x23'-'\x26' '\x28'-'\x5b' '\x5d'-'\x7e' '\x80'-'\xaf' '\xe0'-'\xf0']

rule lexer = parse
    | "and"              { T_Andfun  } 
    | "let"              { T_Let     }
    | "in"               { T_In      }
    | "rec"              { T_Rec     }
    | "array"            { T_Array   }
    | "dim"              { T_Dim     }
    | "begin"            { T_Begin   }
    | "end"              { T_End     }
    | "delete"           { T_Delete  }
    | "new"              { T_New     }
    | "do"               { T_Do      }
    | "done"             { T_Done    }
    | "downto"           { T_Downto  }
    | "for"              { T_For     }
    | "to"               { T_To      }
    | "while"            { T_While   }
    | "if"               { T_If      }
    | "then"             { T_Then    }
    | "else"             { T_Else    }
    | "match"            { T_Match   }
    | "with"             { T_With    }
    | "mutable"          { T_Mutable }
    | "ref"              { T_Ref     }
    | "of"               { T_Of      }
    | "type"             { T_Type    }
    | "bool"             { T_Tbool   }
    | "char"             { T_Tchar   }
    | "float"            { T_Tfloat  }
    | "int"              { T_Tint    }
    | "unit"             { T_Tunit   }
    | "true"             { T_True }
    | "false"            { T_False }
    
    | intnum             { T_Int    }
    | floatnum           { T_Float  }
    | "()"               { T_Unit   }

    | conid              { T_Conid }
    | genid              { T_Genid }

    | white              { lexer lexbuf } 
    | '\n'               { incr ln_cnt; lexer lexbuf }
    | "--"[^'\n']*       { incr ln_cnt; lexer lexbuf }
    | "+"                { T_Plus       }
    | "-"                { T_Minus      }
    | "*"                { T_Times      }
    | "/"                { T_Divide     }
    | "mod"              { T_Mod        }
    | "+."               { T_Flplus     }
    | "-."               { T_Flminus    }
    | "*."               { T_Fltimes    }
    | "/."               { T_Fldivide   }
    | "**"               { T_Flpower    }
    | "="                { T_Eq         }
    | ">="               { T_Ge         }
    | "<="               { T_Le         }
    | ">"                { T_Gt         }
    | "<"                { T_Lt         }
    | "<>"               { T_Neq        }
    | "=="               { T_Nateq      }
    | "!="               { T_Natneq     }
    | "not"              { T_Not        }
    | "&&"               { T_And        }
    | "||"               { T_Or         }
    | ":="               { T_Assign     }
    | "!"                { T_Bang       }
    | ";"                { T_Semicolon  }
    | "|"                { T_Pipe       }
    | "->"               { T_Gives      }
    | "["                { T_Lbrack     }
    | "]"                { T_Rbrack     }
    | "("                { T_Lparen     }
    | ")"                { T_Rparen     }
    | ":"                { T_Colon      }
    | ","                { T_Comma      }
    | "'"(esc_seq|cmn_char)"'" { T_Char }
    | '"'(esc_seq|cmn_char)*'"' { T_Cstring }
    
    | "(*"               { comments 0 lexbuf }

    | eof                { T_Eof }
    |  _ as chr          { Printf.eprintf "invalid character: '%c' (ascii: %d)"
                            chr (Char.code chr);
                            lexer lexbuf }
    
and comments level = parse
  | "*)"											{ if level = 0 then lexer lexbuf
													  else comments (level-1) lexbuf }
  | "(*"											{ comments (level+1) lexbuf }
  | '\n'											{ incr ln_cnt; comments level lexbuf }
  | _												{ comments level lexbuf }
  | eof												{ error "Comments are not closed" }
  | white                                           { lexer lexbuf }
  | "'" [^ '\n']* "\n"                              { lexer lexbuf }


{
  let string_of_token token =
    match token with
        | T_Andfun       ->     " T_Andfun    "      
        | T_Let          ->     " T_Let       "
        | T_In           ->     " T_In        "
        | T_Rec          ->     " T_Rec       "
        | T_Array        ->     " T_Array     "
        | T_Dim          ->     " T_Dim       "
        | T_Begin        ->     " T_Begin     "
        | T_End          ->     " T_End       "
        | T_Delete       ->     " T_Delete    "
        | T_New          ->     " T_New       "
        | T_Do           ->     " T_Do        "
        | T_Done         ->     " T_Done      "
        | T_Downto       ->     " T_Downto    "
        | T_For          ->     " T_For       "
        | T_To           ->     " T_To        "
        | T_While        ->     " T_While     "
        | T_If           ->     " T_If        "
        | T_Then         ->     " T_Then      "
        | T_Else         ->     " T_Else      "
        | T_Match        ->     " T_Match     "
        | T_With         ->     " T_With      "
        | T_Mutable      ->     " T_Mutable   "
        | T_Ref          ->     " T_Ref       "
        | T_Of           ->     " T_Of        "
        | T_Type         ->     " T_Type      "
        | T_Tbool        ->     " T_Tbool     "
        | T_Tchar        ->     " T_Tchar     "
        | T_Tfloat       ->     " T_Tfloat    "
        | T_Tint         ->     " T_Tint      "
        | T_Tunit        ->     " T_Tunit     "
        | T_Minus        ->     " T_Minus     "
        | T_Plus         ->     " T_Plus      "
        | T_Times        ->     " T_Times     "
        | T_Divide       ->     " T_Divide    "
        | T_Mod          ->     " T_Mod       "
        | T_Flminus      ->     " T_Flminus   "
        | T_Flplus       ->     " T_Flplus    "
        | T_Fltimes      ->     " T_Fltimes   "
        | T_Fldivide     ->     " T_Fldivide  "
        | T_Flpower      ->     " T_Flpower   "
        | T_Eq           ->     " T_Eq        "
        | T_Ge           ->     " T_Ge        "
        | T_Gt           ->     " T_Gt        "
        | T_Le           ->     " T_Le        "
        | T_Lt           ->     " T_Lt        "
        | T_Neq          ->     " T_Neq       "
        | T_Nateq        ->     " T_Nateq     "
        | T_Natneq       ->     " T_Natneq    "
        | T_And          ->     " T_And       "
        | T_Or           ->     " T_Or        "
        | T_Not          ->     " T_Not       "
        | T_Assign       ->     " T_Assign    "
        | T_Bang         ->     " T_Bang      "
        | T_Semicolon    ->     " T_Semicolon "
        | T_Pipe         ->     " T_Pipe      " 
        | T_Gives        ->     " T_Gives     "
        | T_Lbrack       ->     " T_Lbrack    "
        | T_Rbrack       ->     " T_Rbrack    "
        | T_Lparen       ->     " T_Lparen    "
        | T_Rparen       ->     " T_Rparen    "
        | T_Colon        ->     " T_Colon     "
        | T_Comma        ->     " T_Comma     "
        | T_Eof          ->     " T_Eof       "
        | T_Bool         ->     " T_Bool      "
        | T_True         ->     " T_True      "
        | T_False        ->     " T_False     "
        | T_Char         ->     " T_Char      "
        | T_Float        ->     " T_Float     "
        | T_Int          ->     " T_Int       "
        | T_Unit         ->     " T_Unit      "
        | T_Conid        ->     " T_Conid     "
        | T_Genid        ->     " T_Genid     "
        | T_Cstring      ->     " T_Cstring   "                
                                                          
                                                          
  let main () =                       
      Printf.printf "tiiipota";
      let fn =
          if Array.length Sys.argv > 1
                then open_in (Sys.argv.(1))
                else stdin
      in
        let lexbuf = Lexing.from_channel fn in
   (*   let lexbuf = Lexing.from_channxbuf = Lexirom_channel sel stdin in*)
        let rec loop () =
            let token = lexer lexbuf in
                Printf.printf "token=%s, lexeme=\"%s\"\n"
                    (string_of_token token) (Lexing.lexeme lexbuf);
                    if token <> T_Eof then loop () in
        loop ()
}
