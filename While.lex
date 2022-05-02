structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token
val pos = ref 0
val eof =  fn fileName => Tokens.EOF(!pos,!pos)
val error : string * string * int -> unit = fn
       (fileName,bad,pos) =>
       TextIO.output(TextIO.stdOut,fileName^"["
^Int.toString pos^".] Invalid character \""^bad^"\"\n");
type lexarg = string
type arg = lexarg
fun intFromString str =
    case Int.fromString str of
         SOME num => num
       | NONE => raise Fail ("Could not convert string '" ^ str ^ "' to int!")



(* Keyword management *)
(* structure KeyWord :
sig val find : string ->
(int * int -> (svalue,int) token) option
end =
struct
val TableSize = 422 (* 211 *)
val HashFactor = 5
val hash = fn
s => List.foldr (fn (c,v) =>
(v*HashFactor+(ord c)) mod TableSize) 0 (explode s)
val HashTable = Array.array(TableSize,nil) :
(string * (int * int -> (svalue,int) token))
list Array.array
val add = fn
(s,v) => let val i = hash s
in Array.update(HashTable,i,(s,v)
:: (Array.sub(HashTable, i)))
end
val find = fn
s => let val i = hash s
fun f ((key,v)::r) = if s=key then SOME v
else f r
| f nil = NONE
in f (Array.sub(HashTable, i))
end
val _ = (List.app add [
("program", Tokens.PROG),
("var", Tokens.VAR),
("int", Tokens.INT),
("bool", Tokens.BOOL),
("read", Tokens.READ),
("write", Tokens.WRITE),
("if", Tokens.IF),
("then", Tokens.THEN),
("else", Tokens.ELSE),
("endif", Tokens.ENDIF),
("while", Tokens.WH),
("do", Tokens.DO),
("endwh", Tokens.ENDWH)
])
end;
open KeyWord; *)




%%
%header (functor WhileLexFun(structure Tokens: While_TOKENS)); 
%arg (fileName:string);
Letter=[A-Za-z];
Digit=[0-9];
Random = [A-Za-z0-9];
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; continue());
{ws}+    => (continue());
"+"      => (print ("---"^yytext^"---");Tokens.PLUS(!pos,!pos));
"-"      => (print ("---"^yytext^"---");Tokens.MINUS(!pos,!pos));
"*"      => (print ("---"^yytext^"---");Tokens.TIMES(!pos,!pos));
"/"      => (print ("---"^yytext^"---");Tokens.DIV(!pos,!pos));
"%"      => (print ("---"^yytext^"---");Tokens.MOD(!pos,!pos));
">"      => (print ("---"^yytext^"---");Tokens.GT(!pos,!pos));
"<"      => (print ("---"^yytext^"---");Tokens.LT(!pos,!pos));
">="      => (print ("---"^yytext^"---");Tokens.GEQ(!pos,!pos));
"<="      => (print ("---"^yytext^"---");Tokens.LEQ(!pos,!pos));
"="      => (print ("---"^yytext^"---");Tokens.EQ(!pos,!pos));
"<>"      => (print ("---"^yytext^"---");Tokens.NEQ(!pos,!pos));
"{"      => (print ("---"^yytext^"---");Tokens.LEFTCURL(!pos,!pos));
"}"      => (print ("---"^yytext^"---");Tokens.RIGHTCURL(!pos,!pos));
"("      => (print ("---"^yytext^"---");Tokens.LEFTPAREN(!pos,!pos));
")"      => (print ("---"^yytext^"---");Tokens.RIGHTPAREN(!pos,!pos));
"&&"      => (print ("---"^yytext^"---");Tokens.AND(!pos,!pos));
"||"      => (print ("---"^yytext^"---");Tokens.OR(!pos,!pos));
":="      => (print ("---"^yytext^"---");Tokens.SET(!pos,!pos));
"::"      => (print("--::--");Tokens.SEQ(!pos,!pos));
":"      => (print ("---"^yytext^"---");Tokens.COLON(!pos,!pos));
";"      => (print ("---"^yytext^"---");Tokens.SEMICOLON(!pos,!pos));
","      => (print ("---"^yytext^"---");Tokens.COMMA(!pos,!pos));
"!"      => (print ("---"^yytext^"---");Tokens.NOT(!pos,!pos));
"~"      => (print ("---"^yytext^"---");Tokens.NEGATE(!pos,!pos));
"tt"      => (print ("---"^yytext^"---");Tokens.TRUE(true,!pos,!pos));
"ff"      => (print ("---"^yytext^"---");Tokens.FALSE(false,!pos,!pos));
"program"=> (print ("---program---");Tokens.PROG(!pos,!pos));
"var"    => (print ("---"^yytext^"---");Tokens.VAR(!pos,!pos));
"int"      => (print ("---"^yytext^"---");Tokens.INT(yytext,!pos,!pos));
"bool"      => (print ("---"^yytext^"---");Tokens.BOOL(yytext,!pos,!pos));
"read"      => (print ("---read---");Tokens.READ(!pos,!pos));
"write"      => (print ("---write---");Tokens.WRITE(!pos,!pos));
"if"      => (print ("---if---");Tokens.IF(!pos,!pos));
"then"      => (print ("---then---");Tokens.THEN(!pos,!pos));
"else"      => (print ("---else---");Tokens.ELSE(!pos,!pos));
"endif"      => (print ("---endif---");Tokens.ENDIF(!pos,!pos));
"while"      => (print ("---while---");Tokens.WH(!pos,!pos));
"do"      => (print ("---do---");Tokens.DO(!pos,!pos));
"endwh"      => (print ("---endwh---");Tokens.ENDWH(!pos,!pos));
{Letter}{Random}*  => (print ("---"^yytext^"---");Tokens.ID(yytext, !pos,!pos));
{Digit}+ => (print ("---"^yytext^"---");Tokens.NUMBER(intFromString yytext, !pos,!pos));
.      =>(error ( fileName,yytext,!pos);continue());