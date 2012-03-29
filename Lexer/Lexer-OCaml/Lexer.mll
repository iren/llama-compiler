(*Lydia Zakynthinou (03108024) & Vlassi-Pandi Eirini (03108137)*)
(*                         llama-Lexer                         *)

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
        | T_New         
        | T_Delete      
        | T_While       
        | T_For         
        | T_To          
        | T_Downto      
        | T_Do          
        | T_Done        
        | T_If          
        | T_Then        
        | T_Else        
        | T_Match       
        | T_With        
        | T_Mutable     
        | T_Ref         
        | T_Of          
        | T_Type        
        | T_BoolType       
        | T_CharType       
        | T_FloatType      
        | T_IntType        
        | T_UnitType       
        | T_Plus        
        | T_Minus       
        | T_Times       
        | T_Div      
        | T_Mod         
        | T_MinusF     
        | T_PlusF    
        | T_TimesF    
        | T_DivF    
        | T_Power     
        | T_Equals          
        | T_GreaterEq          
        | T_Greater          
        | T_LessEq     
        | T_Less      
        | T_NotEq         
        | T_NaturalEq       
        | T_NaturalNotEq      
        | T_And         
        | T_Or          
        | T_Not         
        | T_AssignPointer      
        | T_BangPointer        
        | T_Semicolon   
        | T_Pipe      
        | T_Arrow       
        | T_LeftBr      
        | T_RightBr      
        | T_LeftPar      
        | T_RightPar     
        | T_Colon       
        | T_Comma       
        | T_Eof                      
        | T_True
        | T_False
        | T_Char        
        | T_Float       
        | T_Int         
        | T_Unit                   
        | T_Constructor       
        | T_Identifier       
        | T_String            

    let error error_string = 
        Printf.eprintf "ERROR in line %d: %s\n"  !ln_cnt error_string; exit 1 
}

    let digit    = ['0'-'9']
    let integers = ['0'-'9']+
    let floats   = integers"."integers(('e'|'E')(('+'|'-')?)integers)?
                   
    let lowerCase   = ['a'-'z']
    let upperCase   = ['A'-'Z']
    let idChar      = ['a'-'z''A'-'Z''0'-'9''_']
                   
    let identifier  = lowerCase idChar*
    let constructor = upperCase idChar*

    let white    = [' ''\t']+
    let eol      = ['\r''\n']|"\r\n"
   
    let esc_seq  = ("\\n") | "\\t" | "\\r" | "\\0" | "\\\\" | "\\'" |"\\\"" | ("\\x"['0'-'9''a'-'f''A'-'F']['0'-'9''a'-'f''A'-'F'])
    let cmn_char = ['\x20'-'\x21'  '\x23'-'\x26' '\x28'-'\x5b' '\x5d'-'\x7e' '\x80'-'\xaf' '\xe0'-'\xf0']

rule lexer = parse
    | "and"                      { T_Andfun } 
    | "let"                      { T_Let }
    | "in"                       { T_In }
    | "rec"                      { T_Rec }
    | "array"                    { T_Array }
    | "dim"                      { T_Dim }
    | "begin"                    { T_Begin }
    | "end"                      { T_End }
    | "new"                      { T_New }
    | "delete"                   { T_Delete }
    | "while"                    { T_While }
    | "for"                      { T_For }
    | "to"                       { T_To }
    | "downto"                   { T_Downto }
    | "do"                       { T_Do }
    | "done"                     { T_Done }
    | "if"                       { T_If }
    | "then"                     { T_Then }
    | "else"                     { T_Else }
    | "match"                    { T_Match }
    | "with"                     { T_With }
    | "mutable"                  { T_Mutable }
    | "ref"                      { T_Ref }
    | "of"                       { T_Of }
    | "type"                     { T_Type }
    | "bool"                     { T_BoolType }
    | "char"                     { T_CharType }
    | "float"                    { T_FloatType }
    | "int"                      { T_IntType }
    | "unit"                     { T_UnitType }
    | "true"                     { T_True }
    | "false"                    { T_False }
    
    | integers                   { T_Int    }
    | floats                     { T_Float  }
    | "()"                       { T_Unit   }

    | constructor                { T_Constructor }
    | identifier                 { T_Identifier }

    | white                      { lexer lexbuf } 
    | eol                        { incr ln_cnt; lexer lexbuf }
    | "--"[^'\n']* "\n"          { incr ln_cnt; lexer lexbuf }
    | "+"                        { T_Plus }
    | "-"                        { T_Minus }
    | "*"                        { T_Times }
    | "/"                        { T_Div }
    | "mod"                      { T_Mod }
    | "+."                       { T_PlusF }
    | "-."                       { T_MinusF }
    | "*."                       { T_TimesF }
    | "/."                       { T_DivF }
    | "**"                       { T_Power }
    | "="                        { T_Equals }
    | ">="                       { T_GreaterEq }
    | "<="                       { T_Greater }
    | ">"                        { T_LessEq }
    | "<"                        { T_Less }
    | "<>"                       { T_NotEq }
    | "=="                       { T_NaturalEq }
    | "!="                       { T_NaturalNotEq }
    | "&&"                       { T_And }
    | "||"                       { T_Or }
    | "not"                      { T_Not }
    | ":="                       { T_AssignPointer }
    | "!"                        { T_BangPointer }
    | ";"                        { T_Semicolon }
    | "|"                        { T_Pipe }
    | "->"                       { T_Arrow }
    | "["                        { T_LeftBr }
    | "]"                        { T_RightBr }
    | "("                        { T_LeftPar }
    | ")"                        { T_RightPar }
    | ":"                        { T_Colon }
    | ","                        { T_Comma }
    | "'"(esc_seq|cmn_char)"'"   { T_Char }
    | '"'(esc_seq|cmn_char)*'"'  { T_String }
    
    
    | "(*"                       { comments 0 lexbuf }

    
    | eof                       { T_Eof }
    |  _ as chr                 { Printf.printf "Invalid character: '%c' (ascii: %d) in line %d \n"
                                    chr (Char.code chr) !ln_cnt;
                                    exit 1 }
   
and comments level = parse
    | "*)"						{ if level = 0 then lexer lexbuf
                                    else comments (level-1) lexbuf }
    | "(*"						{ comments (level+1) lexbuf }
    | eol						{ incr ln_cnt; comments level lexbuf }
    | eof						{ decr ln_cnt; error "Comments are not closed\n" }
    | _							{ comments level lexbuf }


{
  let string_of_token token =
    match token with
        | T_Andfun         ->     " T_Andfun      "      
        | T_Let            ->     " T_Let         "
        | T_In             ->     " T_In          "
        | T_Rec            ->     " T_Rec         "
        | T_Array          ->     " T_Array       "
        | T_Dim            ->     " T_Dim         "
        | T_Begin          ->     " T_Begin       "
        | T_End            ->     " T_End         "
        | T_New            ->     " T_New         "
        | T_Delete         ->     " T_Delete      "
        | T_While          ->     " T_While       "
        | T_For            ->     " T_For         "
        | T_To             ->     " T_To          "
        | T_Downto         ->     " T_Downto      "
        | T_Do             ->     " T_Do          "
        | T_Done           ->     " T_Done        "
        | T_If             ->     " T_If          "
        | T_Then           ->     " T_Then        "
        | T_Else           ->     " T_Else        "
        | T_Match          ->     " T_Match       "
        | T_With           ->     " T_With        "
        | T_Mutable        ->     " T_Mutable     "
        | T_Ref            ->     " T_Ref         "
        | T_Of             ->     " T_Of          "
        | T_Type           ->     " T_Type        "
        | T_BoolType       ->     " T_BoolType    "
        | T_CharType       ->     " T_CharType    "
        | T_FloatType      ->     " T_FloatType   "
        | T_IntType        ->     " T_IntType     "
        | T_UnitType       ->     " T_UnitType    "
        | T_Plus           ->     " T_Plus        "
        | T_Minus          ->     " T_Minus       "
        | T_Times          ->     " T_Times       "
        | T_Div            ->     " T_Div         "
        | T_Mod            ->     " T_Mod         "
        | T_MinusF         ->     " T_MinusF      "
        | T_PlusF          ->     " T_PlusF       "
        | T_TimesF         ->     " T_TimesF      "
        | T_DivF           ->     " T_DivF        "
        | T_Power          ->     " T_Power       "
        | T_Equals         ->     " T_Equals      "
        | T_GreaterEq      ->     " T_GreaterEq   "
        | T_Greater        ->     " T_Greater     "
        | T_LessEq         ->     " T_LessEq      "
        | T_Less           ->     " T_Less        "
        | T_NotEq          ->     " T_NotEq       "
        | T_NaturalEq      ->     " T_NaturalEq   "
        | T_NaturalNotEq   ->     " T_NaturalNotEq"
        | T_And            ->     " T_And         "
        | T_Or             ->     " T_Or          "
        | T_Not            ->     " T_Not         "
        | T_AssignPointer  ->     " T_AssignPointer"
        | T_BangPointer    ->     " T_BangPointer "
        | T_Semicolon      ->     " T_Semicolon   "
        | T_Pipe           ->     " T_Pipe        " 
        | T_Arrow          ->     " T_Arrow       "
        | T_LeftBr         ->     " T_LeftBr      "
        | T_RightBr        ->     " T_RightBr     "
        | T_LeftPar        ->     " T_LeftPar     "
        | T_RightPar       ->     " T_RightPar    "
        | T_Colon          ->     " T_Colon       "
        | T_Comma          ->     " T_Comma       "
        | T_Eof            ->     " T_Eof         "
        | T_True           ->     " T_True        "
        | T_False          ->     " T_False       "
        | T_Char           ->     " T_Char        "
        | T_Float          ->     " T_Float       "
        | T_Int            ->     " T_Int         "
        | T_Unit           ->     " T_Unit        "
        | T_Constructor    ->     " T_Constructor "
        | T_Identifier     ->     " T_Identifier  "
        | T_String         ->     " T_String      "
                                                          
                                                          
  let main () =                       
      let fn =
          if Array.length Sys.argv > 1
                then open_in (Sys.argv.(1))
                else stdin
      in
        let lexbuf = Lexing.from_channel fn in
        let rec loop () =
            let token = lexer lexbuf in
                Printf.printf "token=%s, lexeme=\"%s\"\n"
                    (string_of_token token) (Lexing.lexeme lexbuf);
                    if token <> T_Eof then loop () in
        loop ()

  let _ = main ();
}
