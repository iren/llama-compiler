{
    open Lexing
    type linePosT = int
    type charPosT = int
    type token
        = T_Andcomb      of linePosT * charPosT
        | T_In           of linePosT * charPosT
        | T_Let          of linePosT * charPosT
        | T_Rec          of linePosT * charPosT
        | T_Array        of linePosT * charPosT
        | T_Dim          of linePosT * charPosT
        | T_Begin        of linePosT * charPosT
        | T_End          of linePosT * charPosT
        | T_Delete       of linePosT * charPosT
        | T_New          of linePosT * charPosT
        | T_Do           of linePosT * charPosT
        | T_Done         of linePosT * charPosT
        | T_Donwto       of linePosT * charPosT
        | T_For          of linePosT * charPosT
        | T_To           of linePosT * charPosT
        | T_While        of linePosT * charPosT
        | T_Else         of linePosT * charPosT
        | T_If           of linePosT * charPosT
        | T_Then         of linePosT * charPosT
        | T_Match        of linePosT * charPosT
        | T_With         of linePosT * charPosT
        | T_Mutable      of linePosT * charPosT
        | T_Ref          of linePosT * charPosT
        | T_Of           of linePosT * charPosT
        | T_Type         of linePosT * charPosT
        | T_Tbool        of linePosT * charPosT
        | T_Tchar        of linePosT * charPosT
        | T_Tfloat       of linePosT * charPosT
        | T_Tint         of linePosT * charPosT
        | T_Tunit        of linePosT * charPosT
        | T_Minus        of linePosT * charPosT
        | T_Plus         of linePosT * charPosT
        | T_Times        of linePosT * charPosT
        | T_Divide       of linePosT * charPosT
        | T_Mod          of linePosT * charPosT
        | T_Flminus      of linePosT * charPosT
        | T_Flplus       of linePosT * charPosT
        | T_Fltimes      of linePosT * charPosT
        | T_Fldivide     of linePosT * charPosT
        | T_Flpower      of linePosT * charPosT
        | T_Eq           of linePosT * charPosT
        | T_Ge           of linePosT * charPosT
        | T_Gt           of linePosT * charPosT
        | T_Le           of linePosT * charPosT
        | T_Lt           of linePosT * charPosT
        | T_Neq          of linePosT * charPosT
        | T_Nateq        of linePosT * charPosT
        | T_Natneq       of linePosT * charPosT
        | T_Not          of linePosT * charPosT
        | T_And          of linePosT * charPosT
        | T_Or           of linePosT * charPosT
        | T_Assign       of linePosT * charPosT
        | T_Bang         of linePosT * charPosT
        | T_Semicolon    of linePosT * charPosT
        | T_Souvlaki     of linePosT * charPosT
        | T_Gives        of linePosT * charPosT
        | T_Lbrack       of linePosT * charPosT
        | T_Rbrack       of linePosT * charPosT
        | T_Lparen       of linePosT * charPosT
        | T_Rparen       of linePosT * charPosT
        | T_Colon        of linePosT * charPosT
        | T_Comma        of linePosT * charPosT
        | T_Eof          of linePosT * charPosT
        | T_Bool         of bool   * linePosT * charPosT
        | T_Char         of string * linePosT * charPosT (* TODO Is conversion to char useful ? *)
        | T_Float        of float  * linePosT * charPosT
        | T_Int          of int    * linePosT * charPosT
        | T_Unit         of unit   * linePosT * charPosT
        | T_Conid        of string * linePosT * charPosT
        | T_Genid        of string * linePosT * charPosT
        | T_Cstring      of string * linePosT * charPosT

    let fileP () = if Array.length Sys.argv > 0
        then Sys.argv.(1)
        else "<stdin>"
    let lineP lexbuf = (Lexing.lexeme_start_p lexbuf).pos_lnum
    let chrP  lexbuf =
        let
            lPos = Lexing.lexeme_start_p lexbuf
        in
            lPos.pos_cnum - lPos.pos_bol
}

let ws       = [' ''\t']+
let eol      = ['\r''\n']|"\r\n"
               
let digit    = ['0'-'9']
let hexdigit = ['0'-'9''a'-'f''A'-'F']
let intnum   = ['0'-'9']+
let floatnum = intnum"."intnum(('e'|'E')(('+'|'-')?)intnum)?
let hexpair  = (hexdigit)(hexdigit)
               
let lowChar = ['a'-'z']
let uppChar = ['A'-'Z']
let idChar  = ['a'-'z''_''A'-'Z''0'-'9']
let symChar = [' ' '!' '#' '$' '%' '^' '&' '*' '(' ')' '-' '=' '+' ':' ';' '[' '{' ']' '}' '|' ',' '<' '.' '>' '/' '?' '`' '~' '@']
let oneChar = symChar | idChar
let squote   = '\''
               
let genid    = lowChar idChar*
let conid    = uppChar idChar*

rule initial = parse
      "and"              { T_Andcomb   (lineP lexbuf, chrP lexbuf) }
    | "in"               { T_In        (lineP lexbuf, chrP lexbuf) }
    | "let"              { T_Let       (lineP lexbuf, chrP lexbuf) }
    | "rec"              { T_Rec       (lineP lexbuf, chrP lexbuf) }
    | "array"            { T_Array     (lineP lexbuf, chrP lexbuf) }
    | "dim"              { T_Dim       (lineP lexbuf, chrP lexbuf) }
    | "begin"            { T_Begin     (lineP lexbuf, chrP lexbuf) }
    | "end"              { T_End       (lineP lexbuf, chrP lexbuf) }
    | "delete"           { T_Delete    (lineP lexbuf, chrP lexbuf) }
    | "new"              { T_New       (lineP lexbuf, chrP lexbuf) }
    | "do"               { T_Do        (lineP lexbuf, chrP lexbuf) }
    | "done"             { T_Done      (lineP lexbuf, chrP lexbuf) }
    | "downto"           { T_Donwto    (lineP lexbuf, chrP lexbuf) }
    | "for"              { T_For       (lineP lexbuf, chrP lexbuf) }
    | "to"               { T_To        (lineP lexbuf, chrP lexbuf) }
    | "while"            { T_While     (lineP lexbuf, chrP lexbuf) }
    | "else"             { T_Else      (lineP lexbuf, chrP lexbuf) }
    | "if"               { T_If        (lineP lexbuf, chrP lexbuf) }
    | "then"             { T_Then      (lineP lexbuf, chrP lexbuf) }
    | "match"            { T_Match     (lineP lexbuf, chrP lexbuf) }
    | "with"             { T_With      (lineP lexbuf, chrP lexbuf) }
    | "mutable"          { T_Mutable   (lineP lexbuf, chrP lexbuf) }
    | "ref"              { T_Ref       (lineP lexbuf, chrP lexbuf) }
    | "of"               { T_Of        (lineP lexbuf, chrP lexbuf) }
    | "type"             { T_Type      (lineP lexbuf, chrP lexbuf) }
    | "bool"             { T_Tbool     (lineP lexbuf, chrP lexbuf) }
    | "char"             { T_Tchar     (lineP lexbuf, chrP lexbuf) }
    | "float"            { T_Tfloat    (lineP lexbuf, chrP lexbuf) }
    | "int"              { T_Tint      (lineP lexbuf, chrP lexbuf) }
    | "unit"             { T_Tunit     (lineP lexbuf, chrP lexbuf) }
    | "-"                { T_Minus     (lineP lexbuf, chrP lexbuf) }
    | "+"                { T_Plus      (lineP lexbuf, chrP lexbuf) }
    | "*"                { T_Times     (lineP lexbuf, chrP lexbuf) }
    | "/"                { T_Divide    (lineP lexbuf, chrP lexbuf) }
    | "mod"              { T_Mod       (lineP lexbuf, chrP lexbuf) }
    | "-."               { T_Flminus   (lineP lexbuf, chrP lexbuf) }
    | "+."               { T_Flplus    (lineP lexbuf, chrP lexbuf) }
    | "*."               { T_Fltimes   (lineP lexbuf, chrP lexbuf) }
    | "/."               { T_Fldivide  (lineP lexbuf, chrP lexbuf) }
    | "**"               { T_Flpower   (lineP lexbuf, chrP lexbuf) }
    | "="                { T_Eq        (lineP lexbuf, chrP lexbuf) }
    | ">="               { T_Ge        (lineP lexbuf, chrP lexbuf) }
    | "<="               { T_Le        (lineP lexbuf, chrP lexbuf) }
    | ">"                { T_Gt        (lineP lexbuf, chrP lexbuf) }
    | "<"                { T_Lt        (lineP lexbuf, chrP lexbuf) }
    | "<>"               { T_Neq       (lineP lexbuf, chrP lexbuf) }
    | "=="               { T_Nateq     (lineP lexbuf, chrP lexbuf) }
    | "!="               { T_Natneq    (lineP lexbuf, chrP lexbuf) }
    | "not"              { T_Not       (lineP lexbuf, chrP lexbuf) }
    | "&&"               { T_And       (lineP lexbuf, chrP lexbuf) }
    | "||"               { T_Or        (lineP lexbuf, chrP lexbuf) }
    | ":="               { T_Assign    (lineP lexbuf, chrP lexbuf) }
    | "!"                { T_Bang      (lineP lexbuf, chrP lexbuf) }
    | ";"                { T_Semicolon (lineP lexbuf, chrP lexbuf) }
    | "|"                { T_Souvlaki  (lineP lexbuf, chrP lexbuf) }
    | "->"               { T_Gives     (lineP lexbuf, chrP lexbuf) }
    | "["                { T_Lbrack    (lineP lexbuf, chrP lexbuf) }
    | "]"                { T_Rbrack    (lineP lexbuf, chrP lexbuf) }
    | "("                { T_Lparen    (lineP lexbuf, chrP lexbuf) }
    | ")"                { T_Rparen    (lineP lexbuf, chrP lexbuf) }
    | ":"                { T_Colon     (lineP lexbuf, chrP lexbuf) }
    | ","                { T_Comma     (lineP lexbuf, chrP lexbuf) }
    | "true"             { T_Bool      (true,  lineP lexbuf, chrP lexbuf) }
    | "false"            { T_Bool      (false, lineP lexbuf, chrP lexbuf) }

    | intnum   as inum   { T_Int   (int_of_string inum,   lineP lexbuf, chrP lexbuf) }
    | floatnum as fnum   { T_Float (float_of_string fnum, lineP lexbuf, chrP lexbuf) }
    | "()"               { T_Unit  ((),                   lineP lexbuf, chrP lexbuf) }

    | conid as s         { T_Conid (s,                    lineP lexbuf, chrP lexbuf) }
    | genid as s         { T_Genid (s,                    lineP lexbuf, chrP lexbuf) }

    | "--"[^'\n']*       { initial lexbuf }
    | "(*"               { comment 1 lexbuf }

    | '\''               { charconst lexbuf }

    | '"'                { stringconst lexbuf }

    | ws                 { initial lexbuf}
    | eol                { new_line lexbuf; initial lexbuf }
    | eof                { T_Eof (lineP lexbuf, chrP lexbuf) }
    | _ as chr           { Printf.eprintf "%s: %d.%d: Error: Invalid character: '%c' (ASCII: %d)\n"
                            (fileP ()) (lineP lexbuf) (chrP lexbuf) chr (Char.code chr);
                           initial lexbuf
                         }       
and comment n = parse
      [^'(''*''\n''\r']* { comment n lexbuf }
    | "(*"               { comment (n + 1) lexbuf }
    | "*)"               { if n > 1
                            then comment (n - 1) lexbuf
                            else initial lexbuf
                         }
    | eol                { new_line lexbuf; comment n lexbuf }
    | eof                { Printf.eprintf "%s: Warning: Unclosed comment reaching end of file!\n" (fileP ()) ;
                           T_Eof(lineP lexbuf, chrP lexbuf)
                         }
    | _                  { comment n lexbuf }
and charconst = parse
      oneChar 
    | '\\'['n''t''r''\'''"''0']squote
    | "\\\\"squote
    | '"'squote as s    {   let news = String.sub s 0 (String.length s - 1) in
                                T_Char (news, lineP lexbuf, chrP lexbuf)
                        }
    | "\\x"(hexpair)squote as s     {   let news = String.sub s 0 (String.length s - 1) in
                                            T_Char (s, lineP lexbuf, chrP lexbuf)
                                    }
    | squote            { Printf.eprintf "%s: %d.%d: Error: Empty character not allowed!\n" (fileP ()) (lineP lexbuf) (chrP lexbuf);
                          initial lexbuf
                        }
    | _                 { Printf.eprintf "%s: %d.%d: Error: Non-character sequence.\n" (fileP ()) (lineP lexbuf) (chrP lexbuf);
                          initial lexbuf
                        }
and stringconst = parse
    | ("\\\""|[^'"''\n''\r'])*'"' as s  {
                                            let news = String.sub s 0 (String.length s - 1) in
                                                T_Cstring (news, lineP lexbuf, chrP lexbuf)
                                        }
    | ("\\\""|[^'"''\n''\r'])*          { stringconst lexbuf }
    | eol               { Printf.eprintf "%s: %d.%d: Error: String spanning multiple lines.\n" (fileP ()) (lineP lexbuf) (chrP lexbuf); 
                          new_line lexbuf;
                          searchdquote lexbuf
                        } 
and searchdquote = parse
    | eol               { new_line lexbuf;
                          searchdquote lexbuf
                        } 
    | eof               { Printf.eprintf "%s: Error: Unclosed string reaching end of file.\n" (fileP ());
                          T_Eof (lineP lexbuf, chrP lexbuf)
                        }
    | '"'               { T_Cstring("", lineP lexbuf, chrP lexbuf) }
    | _                 { searchdquote lexbuf }

{
    let get_token lexbuf = initial lexbuf
            
    let get_all_tokens lexbuf =
        let rec get_tokens () =
            match initial lexbuf with
                 T_Eof(a, b) -> [T_Eof(a, b)]
                | t          -> t :: get_tokens ()
        in
            get_tokens ()

    let main () = 
        let cin =
            if Array.length Sys.argv > 1
                then open_in (Sys.argv.(1))
                else stdin
        in
            get_all_tokens (Lexing.from_channel cin)

    let _ = main ()
}
