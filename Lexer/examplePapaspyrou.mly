%token T_eof
%token T_print
%token T_let
%token T_for
....
%token T_num
%token T_var
%token T_plus
%token T_times
%token T_eq /*==*/
%token T_lt
%token T_assign /*anathesh theroume oti isothta kai anathesh =*/ 
%token T_lparen
%token T_rparen

/*prtiorities apo katw pros ta panw (pio ishyro)*/
%nonassoc T_eq T_lt
%left T_plus T_minus
....

%start program 
%type <unit> program
/*mporw na ehw perissotera starting points kai tha ithela kai polla main, gia
kathe start ftiaxnei mia synarthsh me 2 parametrous ? t onoma kai t lexbuff ?  //o
bison den mporei, tha thelame 2 arheia*/

%%

program:   stmt_list T_eof?? { () } ; 

stmt_list: /*nothing*/ { () }  /*epeidh ehoume synartisiakh vgazei lathos kai prepei na
             valoume pantou actions*/
         | stmt_list stmt ;

stmt:      T_print exp { () }
                {Printf.printf"print found\n"}   
         | T_let T_var T_do T_assigb expr { () } 
         | T_for expr T_do stmt { () } 
         | T_if expr T_then stm { () }t
         ...;

expr :   T_num /*ta prwta 3 den eina diforoumena*/
       | T_var
       | T_lparen expr T_rparen
       | expr T_plus expr
       | expr T_minus expr
       | expr T_times expr
       | expr T_eq expr
       | expr T_lt expr;
/*omws einai diforoumenh theloume priorities */
%%
let main =
    let lexbuf = Lexing.from_channel stdin in
    try
        /*let asts = Parser.program Lexer.lexer lexbuf in */
        program Lexer.lexer lexbuf  
        exit 0
    with Parsing.Parse_error -> /*exception pou scaei otan kanoume lathos*/
        Printf.eprintf "syntax error\n"; /*theloume na xeroume se poia grammh
        einai t lathos*/
        exit 1


//////// ocamlyacc Parser.mly
////ftiahnei telika  Parser.mli <---- interface 
type token = .... kai 
val program : ...->unit 

kai t file Parser.ml
///wc -l entolh gia poses grammes ehei kathe arheio

---------------------------lexer--------------------------------------
{
    open Parser
}


"print" {T_print}
"let" {T_let} /*epeidh einai synartisiakh den hreiazetai return*/
"for"  ...
        
['0'-'9']+ {T_num}
['a'-'z'] {T_var}   
"+" {T_plus}
T_times
T_eq /*=
T_lt
T_assign
T_lparen
T_rparen

%%
rule lexer = parse
    "print" {}
   | "let" {T_let}
   |white+ {lexer lexbuf}
   | "'" [^'\n']* "\n" {lexer lexbuf} /*synehizoume anadromika*/

   |eof {T_eof}
   | _ as chr {Printf.eprintf " (ascii:%d)"}


   -----> t  ocamellex Lexer.mll tha friaxei Lexer.ml
   ocamlc Parser.mli
   ocamlc -c Lexer.ml
   ocamlc -c Parser.ml<-------------

   hamos makefile

OCAML_FLAGS = -g
OCAMLC= ocamlc
OCAMLDEP=ocamldep

%.cmo: %.ml %.mli
    $(OCAMLC) $(OCAMLC_FLAGS) -c $<

    ..... $a $^

  Lexer.ml: Lexer.mll
        ocamellex -o  ...

  -include .depend
  depend: Lexer.ml ..
   $() >.......

   1. make depend
   2. make

   Lexer.mli
   val lexer : Lexing.lexbuf -> Parser.token   /*na t ftiaxoume monoi mas*/


   ./llama < arheiaki me programma /*h seira den einai swsth aparaithta*/
