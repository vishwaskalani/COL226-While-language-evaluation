functor WhileLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : While_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open AST
exception Invalid_dataType;
(* Crating an empty hash table. *)
val ht : (string, string) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (50, Fail "not found");

fun decode_v2(VAR(x)) = x;

fun decode(x) = 
        case x of
        inter(y) => "int" |
        booler(y) => "bool";


fun put_in_symbol_table(l,x,l1)=
        if l = [] then (l1,x)
        else
        let
                val a = HashTable.insert(ht)(decode_v2(hd(l)),decode(x))
        in
                put_in_symbol_table(tl(l),x,[hd(l)]@l1)
        end; 



fun int_data_type_checker(x) = 
        case x of
        NUMEXP(y) => 1 |
        NEGATE(y) => int_data_type_checker(y) |
        PLUS(y,z) => int_data_type_checker(y)*int_data_type_checker(z) |
        MINUS(y,z) => int_data_type_checker(y)*int_data_type_checker(z) |
        TIMES(y,z) => int_data_type_checker(y)*int_data_type_checker(z) |
        DIV(y,z) => int_data_type_checker(y)*int_data_type_checker(z) |
        MOD(y,z) => int_data_type_checker(y)*int_data_type_checker(z) |
        VAREXP(x) => if HashTable.lookup(ht)(decode_v2(x)) = "int" then 1 else 0 |
        _ => 0;

fun bool_data_type_checker(x) = 
        case x of
        BOOLEXP(y) => 1 |
        COMPEXP(y) => 1 |
        NOT(y) => bool_data_type_checker(y) |
        OR(y,z) => bool_data_type_checker(y)*bool_data_type_checker(z) |
        AND(y,z) => bool_data_type_checker(y)*bool_data_type_checker(z) |
        VAREXP(x) => if HashTable.lookup(ht)(decode_v2(x)) = "bool" then 1 else 0 |
        _ => 0;

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\004\000\000\000\
\\001\000\001\000\016\000\000\000\
\\001\000\001\000\016\000\002\000\040\000\003\000\039\000\017\000\038\000\
\\022\000\037\000\023\000\036\000\039\000\035\000\040\000\034\000\000\000\
\\001\000\001\000\016\000\016\000\104\000\028\000\023\000\029\000\022\000\
\\030\000\021\000\034\000\020\000\000\000\
\\001\000\001\000\016\000\016\000\024\000\028\000\023\000\029\000\022\000\
\\030\000\021\000\034\000\020\000\000\000\
\\001\000\002\000\067\000\000\000\
\\001\000\003\000\110\000\004\000\062\000\006\000\110\000\007\000\060\000\
\\008\000\059\000\009\000\110\000\010\000\110\000\011\000\110\000\
\\012\000\110\000\013\000\110\000\014\000\110\000\018\000\110\000\
\\019\000\110\000\020\000\110\000\031\000\110\000\035\000\110\000\
\\041\000\110\000\000\000\
\\001\000\003\000\111\000\004\000\062\000\006\000\111\000\007\000\060\000\
\\008\000\059\000\009\000\111\000\010\000\111\000\011\000\111\000\
\\012\000\111\000\013\000\111\000\014\000\111\000\018\000\111\000\
\\019\000\111\000\020\000\111\000\031\000\111\000\035\000\111\000\
\\041\000\111\000\000\000\
\\001\000\003\000\113\000\004\000\113\000\006\000\113\000\007\000\113\000\
\\008\000\113\000\009\000\113\000\010\000\113\000\011\000\113\000\
\\012\000\113\000\013\000\113\000\014\000\113\000\018\000\113\000\
\\019\000\113\000\020\000\113\000\031\000\113\000\035\000\113\000\
\\041\000\113\000\000\000\
\\001\000\003\000\114\000\004\000\114\000\006\000\114\000\007\000\114\000\
\\008\000\114\000\009\000\114\000\010\000\114\000\011\000\114\000\
\\012\000\114\000\013\000\114\000\014\000\114\000\018\000\114\000\
\\019\000\114\000\020\000\114\000\031\000\114\000\035\000\114\000\
\\041\000\114\000\000\000\
\\001\000\003\000\115\000\004\000\115\000\006\000\115\000\007\000\115\000\
\\008\000\115\000\009\000\115\000\010\000\115\000\011\000\115\000\
\\012\000\115\000\013\000\115\000\014\000\115\000\018\000\115\000\
\\019\000\115\000\020\000\115\000\031\000\115\000\035\000\115\000\
\\041\000\115\000\000\000\
\\001\000\003\000\117\000\004\000\117\000\006\000\117\000\007\000\117\000\
\\008\000\117\000\009\000\117\000\010\000\117\000\011\000\117\000\
\\012\000\117\000\013\000\117\000\014\000\117\000\018\000\117\000\
\\019\000\117\000\020\000\117\000\031\000\117\000\035\000\117\000\
\\041\000\117\000\000\000\
\\001\000\003\000\118\000\004\000\118\000\006\000\118\000\007\000\118\000\
\\008\000\118\000\009\000\118\000\010\000\118\000\011\000\118\000\
\\012\000\118\000\013\000\118\000\014\000\118\000\018\000\118\000\
\\019\000\118\000\020\000\118\000\031\000\118\000\035\000\118\000\
\\041\000\118\000\000\000\
\\001\000\003\000\119\000\004\000\119\000\006\000\119\000\007\000\119\000\
\\008\000\119\000\009\000\119\000\010\000\119\000\011\000\119\000\
\\012\000\119\000\013\000\119\000\014\000\119\000\018\000\119\000\
\\019\000\119\000\020\000\119\000\031\000\119\000\035\000\119\000\
\\041\000\119\000\000\000\
\\001\000\003\000\120\000\004\000\120\000\006\000\120\000\007\000\120\000\
\\008\000\120\000\009\000\120\000\010\000\120\000\011\000\120\000\
\\012\000\120\000\013\000\120\000\014\000\120\000\018\000\120\000\
\\019\000\120\000\020\000\120\000\031\000\120\000\035\000\120\000\
\\041\000\120\000\000\000\
\\001\000\003\000\121\000\004\000\121\000\006\000\121\000\007\000\121\000\
\\008\000\121\000\009\000\121\000\010\000\121\000\011\000\121\000\
\\012\000\121\000\013\000\121\000\014\000\121\000\018\000\121\000\
\\019\000\121\000\020\000\121\000\031\000\121\000\035\000\121\000\
\\041\000\121\000\000\000\
\\001\000\003\000\122\000\004\000\122\000\006\000\122\000\007\000\122\000\
\\008\000\122\000\009\000\122\000\010\000\122\000\011\000\122\000\
\\012\000\122\000\013\000\122\000\014\000\122\000\018\000\122\000\
\\019\000\122\000\020\000\122\000\031\000\122\000\035\000\122\000\
\\041\000\122\000\000\000\
\\001\000\003\000\123\000\004\000\123\000\006\000\123\000\007\000\123\000\
\\008\000\123\000\009\000\123\000\010\000\123\000\011\000\123\000\
\\012\000\123\000\013\000\123\000\014\000\123\000\018\000\123\000\
\\019\000\123\000\020\000\123\000\031\000\123\000\035\000\123\000\
\\041\000\123\000\000\000\
\\001\000\003\000\131\000\004\000\131\000\006\000\131\000\007\000\131\000\
\\008\000\131\000\009\000\131\000\010\000\131\000\011\000\131\000\
\\012\000\131\000\013\000\131\000\014\000\131\000\018\000\131\000\
\\019\000\131\000\020\000\131\000\031\000\131\000\035\000\131\000\
\\037\000\131\000\038\000\131\000\041\000\131\000\043\000\131\000\000\000\
\\001\000\003\000\132\000\004\000\132\000\006\000\132\000\007\000\132\000\
\\008\000\132\000\009\000\132\000\010\000\132\000\011\000\132\000\
\\012\000\132\000\013\000\132\000\014\000\132\000\018\000\132\000\
\\019\000\132\000\020\000\132\000\031\000\132\000\035\000\132\000\
\\041\000\132\000\000\000\
\\001\000\003\000\133\000\004\000\133\000\006\000\133\000\007\000\133\000\
\\008\000\133\000\009\000\133\000\010\000\133\000\011\000\133\000\
\\012\000\133\000\013\000\133\000\014\000\133\000\018\000\133\000\
\\019\000\133\000\020\000\133\000\031\000\133\000\035\000\133\000\
\\041\000\133\000\000\000\
\\001\000\003\000\063\000\004\000\062\000\006\000\061\000\007\000\060\000\
\\008\000\059\000\009\000\125\000\010\000\125\000\011\000\125\000\
\\012\000\125\000\013\000\125\000\014\000\125\000\018\000\125\000\
\\019\000\125\000\020\000\125\000\031\000\125\000\035\000\125\000\
\\041\000\125\000\000\000\
\\001\000\003\000\063\000\004\000\062\000\006\000\061\000\007\000\060\000\
\\008\000\059\000\009\000\126\000\010\000\126\000\011\000\126\000\
\\012\000\126\000\013\000\126\000\014\000\126\000\018\000\126\000\
\\019\000\126\000\020\000\126\000\031\000\126\000\035\000\126\000\
\\041\000\126\000\000\000\
\\001\000\003\000\063\000\004\000\062\000\006\000\061\000\007\000\060\000\
\\008\000\059\000\009\000\127\000\010\000\127\000\011\000\127\000\
\\012\000\127\000\013\000\127\000\014\000\127\000\018\000\127\000\
\\019\000\127\000\020\000\127\000\031\000\127\000\035\000\127\000\
\\041\000\127\000\000\000\
\\001\000\003\000\063\000\004\000\062\000\006\000\061\000\007\000\060\000\
\\008\000\059\000\009\000\128\000\010\000\128\000\011\000\128\000\
\\012\000\128\000\013\000\128\000\014\000\128\000\018\000\128\000\
\\019\000\128\000\020\000\128\000\031\000\128\000\035\000\128\000\
\\041\000\128\000\000\000\
\\001\000\003\000\063\000\004\000\062\000\006\000\061\000\007\000\060\000\
\\008\000\059\000\009\000\058\000\010\000\057\000\011\000\056\000\
\\012\000\055\000\013\000\112\000\014\000\112\000\018\000\112\000\
\\019\000\052\000\020\000\112\000\031\000\112\000\035\000\112\000\
\\041\000\112\000\000\000\
\\001\000\003\000\063\000\004\000\062\000\006\000\061\000\007\000\060\000\
\\008\000\059\000\009\000\058\000\010\000\057\000\011\000\056\000\
\\012\000\055\000\013\000\116\000\014\000\116\000\018\000\116\000\
\\019\000\116\000\020\000\116\000\031\000\116\000\035\000\116\000\
\\041\000\116\000\000\000\
\\001\000\003\000\063\000\004\000\062\000\006\000\061\000\007\000\060\000\
\\008\000\059\000\009\000\058\000\010\000\057\000\011\000\056\000\
\\012\000\055\000\013\000\124\000\014\000\124\000\018\000\124\000\
\\019\000\124\000\020\000\124\000\031\000\124\000\035\000\124\000\
\\041\000\124\000\000\000\
\\001\000\003\000\063\000\004\000\062\000\006\000\061\000\007\000\060\000\
\\008\000\059\000\009\000\058\000\010\000\057\000\011\000\056\000\
\\012\000\055\000\013\000\054\000\014\000\053\000\018\000\129\000\
\\019\000\052\000\020\000\051\000\031\000\129\000\035\000\129\000\
\\041\000\129\000\000\000\
\\001\000\003\000\063\000\004\000\062\000\006\000\061\000\007\000\060\000\
\\008\000\059\000\009\000\058\000\010\000\057\000\011\000\056\000\
\\012\000\055\000\013\000\054\000\014\000\053\000\018\000\130\000\
\\019\000\052\000\020\000\051\000\031\000\130\000\035\000\130\000\
\\041\000\130\000\000\000\
\\001\000\003\000\063\000\004\000\062\000\006\000\061\000\007\000\060\000\
\\008\000\059\000\009\000\058\000\010\000\057\000\011\000\056\000\
\\012\000\055\000\013\000\054\000\014\000\053\000\018\000\084\000\
\\019\000\052\000\020\000\051\000\000\000\
\\001\000\003\000\063\000\004\000\062\000\006\000\061\000\007\000\060\000\
\\008\000\059\000\009\000\058\000\010\000\057\000\011\000\056\000\
\\012\000\055\000\013\000\054\000\014\000\053\000\019\000\052\000\
\\020\000\051\000\031\000\068\000\000\000\
\\001\000\003\000\063\000\004\000\062\000\006\000\061\000\007\000\060\000\
\\008\000\059\000\009\000\058\000\010\000\057\000\011\000\056\000\
\\012\000\055\000\013\000\054\000\014\000\053\000\019\000\052\000\
\\020\000\051\000\035\000\050\000\000\000\
\\001\000\003\000\063\000\004\000\062\000\006\000\061\000\007\000\060\000\
\\008\000\059\000\009\000\058\000\010\000\057\000\011\000\056\000\
\\012\000\055\000\013\000\054\000\014\000\053\000\019\000\052\000\
\\020\000\051\000\041\000\105\000\000\000\
\\001\000\003\000\063\000\004\000\062\000\006\000\061\000\007\000\060\000\
\\008\000\059\000\009\000\058\000\010\000\057\000\011\000\056\000\
\\012\000\055\000\013\000\054\000\014\000\053\000\019\000\052\000\
\\020\000\051\000\041\000\107\000\000\000\
\\001\000\015\000\094\000\000\000\
\\001\000\015\000\095\000\025\000\010\000\000\000\
\\001\000\015\000\096\000\025\000\096\000\000\000\
\\001\000\015\000\011\000\000\000\
\\001\000\015\000\011\000\025\000\010\000\000\000\
\\001\000\016\000\103\000\000\000\
\\001\000\016\000\027\000\000\000\
\\001\000\021\000\005\000\000\000\
\\001\000\024\000\003\000\000\000\
\\001\000\026\000\047\000\027\000\046\000\000\000\
\\001\000\032\000\101\000\033\000\101\000\036\000\101\000\042\000\101\000\000\000\
\\001\000\032\000\102\000\033\000\102\000\036\000\102\000\042\000\102\000\000\000\
\\001\000\032\000\087\000\000\000\
\\001\000\033\000\089\000\000\000\
\\001\000\036\000\086\000\000\000\
\\001\000\037\000\099\000\000\000\
\\001\000\037\000\100\000\038\000\025\000\000\000\
\\001\000\037\000\026\000\000\000\
\\001\000\041\000\097\000\000\000\
\\001\000\041\000\098\000\000\000\
\\001\000\041\000\106\000\000\000\
\\001\000\041\000\108\000\000\000\
\\001\000\041\000\109\000\000\000\
\\001\000\041\000\029\000\000\000\
\\001\000\041\000\069\000\000\000\
\\001\000\042\000\000\000\000\000\
\\001\000\042\000\091\000\000\000\
\\001\000\042\000\092\000\000\000\
\\001\000\042\000\093\000\000\000\
\\001\000\043\000\028\000\000\000\
\"
val actionRowNumbers =
"\043\000\000\000\042\000\039\000\
\\063\000\036\000\038\000\061\000\
\\001\000\004\000\035\000\062\000\
\\051\000\052\000\018\000\041\000\
\\064\000\058\000\002\000\002\000\
\\002\000\001\000\045\000\001\000\
\\044\000\046\000\002\000\003\000\
\\011\000\012\000\017\000\032\000\
\\002\000\002\000\016\000\015\000\
\\002\000\005\000\020\000\031\000\
\\034\000\055\000\050\000\059\000\
\\054\000\053\000\033\000\040\000\
\\038\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\027\000\014\000\
\\030\000\019\000\038\000\037\000\
\\049\000\025\000\026\000\029\000\
\\028\000\023\000\024\000\022\000\
\\021\000\010\000\009\000\007\000\
\\008\000\006\000\013\000\047\000\
\\057\000\038\000\048\000\056\000\
\\060\000"
val gotoT =
"\
\\001\000\088\000\000\000\
\\000\000\
\\000\000\
\\002\000\007\000\003\000\006\000\004\000\005\000\007\000\004\000\000\000\
\\000\000\
\\003\000\010\000\004\000\005\000\000\000\
\\007\000\011\000\000\000\
\\000\000\
\\006\000\013\000\011\000\012\000\000\000\
\\008\000\017\000\011\000\016\000\012\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\031\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\009\000\039\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\009\000\040\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\011\000\041\000\000\000\
\\000\000\
\\006\000\042\000\011\000\012\000\000\000\
\\005\000\043\000\000\000\
\\000\000\
\\009\000\046\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\008\000\017\000\011\000\016\000\012\000\047\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\062\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\009\000\063\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\000\000\
\\000\000\
\\009\000\064\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\068\000\000\000\
\\009\000\069\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\009\000\070\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\009\000\071\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\009\000\072\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\009\000\073\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\009\000\074\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\009\000\075\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\009\000\076\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\009\000\077\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\009\000\078\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\009\000\079\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\009\000\080\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\009\000\081\000\010\000\030\000\011\000\029\000\013\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\083\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\086\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 89
val numrules = 43
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | BOOL of unit ->  (string) | INT of unit ->  (string)
 | FALSE of unit ->  (bool) | TRUE of unit ->  (bool)
 | NUMBER of unit ->  (int) | ID of unit ->  (string)
 | NUM of unit ->  (numer) | NEWVAR1 of unit ->  (cmd list)
 | Variable of unit ->  (vari) | Comparison of unit ->  (comp)
 | Expression of unit ->  (expr) | Command of unit ->  (cmd)
 | CommandSeq of unit ->  (cmd list)
 | VariableList of unit ->  (vari list) | Type of unit ->  (types)
 | Declaration of unit ->  ( ( (vari list)*types ) )
 | DeclarationSeq of unit ->  ( ( (vari list)*types )  list)
 | Block of unit ->  ( ( ((vari list)*types) list ) * ( cmd list ) )
 | Program of unit ->  (PROG)
end
type svalue = MlyValue.svalue
type result = PROG
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 41) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUMBER"
  | (T 2) => "PLUS"
  | (T 3) => "TIMES"
  | (T 4) => "PRINT"
  | (T 5) => "MINUS"
  | (T 6) => "DIV"
  | (T 7) => "MOD"
  | (T 8) => "GT"
  | (T 9) => "LT"
  | (T 10) => "GEQ"
  | (T 11) => "LEQ"
  | (T 12) => "EQ"
  | (T 13) => "NEQ"
  | (T 14) => "LEFTCURL"
  | (T 15) => "RIGHTCURL"
  | (T 16) => "LEFTPAREN"
  | (T 17) => "RIGHTPAREN"
  | (T 18) => "AND"
  | (T 19) => "OR"
  | (T 20) => "SEQ"
  | (T 21) => "TRUE"
  | (T 22) => "FALSE"
  | (T 23) => "PROG"
  | (T 24) => "VAR"
  | (T 25) => "INT"
  | (T 26) => "BOOL"
  | (T 27) => "READ"
  | (T 28) => "WRITE"
  | (T 29) => "IF"
  | (T 30) => "THEN"
  | (T 31) => "ELSE"
  | (T 32) => "ENDIF"
  | (T 33) => "WH"
  | (T 34) => "DO"
  | (T 35) => "ENDWH"
  | (T 36) => "COLON"
  | (T 37) => "COMMA"
  | (T 38) => "NEGATE"
  | (T 39) => "NOT"
  | (T 40) => "SEMICOLON"
  | (T 41) => "EOF"
  | (T 42) => "SET"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36)
 $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29)
 $$ (T 28) $$ (T 27) $$ (T 24) $$ (T 23) $$ (T 20) $$ (T 19) $$ (T 18)
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Block Block1, _, Block1right)) :: _ :: ( _,
 ( MlyValue.ID ID1, _, _)) :: ( _, ( _, PROG1left, _)) :: rest671)) =>
 let val  result = MlyValue.Program (fn _ => let val  (ID as ID1) = 
ID1 ()
 val  (Block as Block1) = Block1 ()
 in (PROG(ID,Block))
end)
 in ( LrTable.NT 0, ( result, PROG1left, Block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.CommandSeq CommandSeq1, _, CommandSeq1right)
) :: ( _, ( MlyValue.DeclarationSeq DeclarationSeq1, 
DeclarationSeq1left, _)) :: rest671)) => let val  result = 
MlyValue.Block (fn _ => let val  (DeclarationSeq as DeclarationSeq1) =
 DeclarationSeq1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in ((DeclarationSeq,CommandSeq))
end)
 in ( LrTable.NT 1, ( result, DeclarationSeq1left, CommandSeq1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.CommandSeq CommandSeq1, CommandSeq1left, 
CommandSeq1right)) :: rest671)) => let val  result = MlyValue.Block
 (fn _ => let val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in (([],CommandSeq))
end)
 in ( LrTable.NT 1, ( result, CommandSeq1left, CommandSeq1right), 
rest671)
end
|  ( 3, ( ( _, ( MlyValue.DeclarationSeq DeclarationSeq1, _, 
DeclarationSeq1right)) :: ( _, ( MlyValue.Declaration Declaration1, 
Declaration1left, _)) :: rest671)) => let val  result = 
MlyValue.DeclarationSeq (fn _ => let val  (Declaration as Declaration1
) = Declaration1 ()
 val  (DeclarationSeq as DeclarationSeq1) = DeclarationSeq1 ()
 in (Declaration::DeclarationSeq)
end)
 in ( LrTable.NT 2, ( result, Declaration1left, DeclarationSeq1right),
 rest671)
end
|  ( 4, ( ( _, ( MlyValue.Declaration Declaration1, Declaration1left, 
Declaration1right)) :: rest671)) => let val  result = 
MlyValue.DeclarationSeq (fn _ => let val  (Declaration as Declaration1
) = Declaration1 ()
 in (Declaration::nil)
end)
 in ( LrTable.NT 2, ( result, Declaration1left, Declaration1right), 
rest671)
end
|  ( 5, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.Type Type1
, _, _)) :: _ :: ( _, ( MlyValue.VariableList VariableList1, _, _)) ::
 ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.Declaration (fn _ => let val  (VariableList as VariableList1)
 = VariableList1 ()
 val  (Type as Type1) = Type1 ()
 in (put_in_symbol_table(VariableList,Type,[]))
end)
 in ( LrTable.NT 3, ( result, VAR1left, SEMICOLON1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.Type (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (inter(INT))
end)
 in ( LrTable.NT 4, ( result, INT1left, INT1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.BOOL BOOL1, BOOL1left, BOOL1right)) :: 
rest671)) => let val  result = MlyValue.Type (fn _ => let val  (BOOL
 as BOOL1) = BOOL1 ()
 in (booler(BOOL))
end)
 in ( LrTable.NT 4, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.VariableList VariableList1, _, 
VariableList1right)) :: _ :: ( _, ( MlyValue.Variable Variable1, 
Variable1left, _)) :: rest671)) => let val  result = 
MlyValue.VariableList (fn _ => let val  (Variable as Variable1) = 
Variable1 ()
 val  (VariableList as VariableList1) = VariableList1 ()
 in (Variable::VariableList)
end)
 in ( LrTable.NT 5, ( result, Variable1left, VariableList1right), 
rest671)
end
|  ( 9, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = 
MlyValue.VariableList (fn _ => let val  (Variable as Variable1) = 
Variable1 ()
 in (Variable::nil)
end)
 in ( LrTable.NT 5, ( result, Variable1left, Variable1right), rest671)

end
|  ( 10, ( ( _, ( _, _, RIGHTCURL1right)) :: ( _, ( _, LEFTCURL1left,
 _)) :: rest671)) => let val  result = MlyValue.CommandSeq (fn _ => (
[]))
 in ( LrTable.NT 6, ( result, LEFTCURL1left, RIGHTCURL1right), rest671
)
end
|  ( 11, ( ( _, ( _, _, RIGHTCURL1right)) :: ( _, ( MlyValue.NEWVAR1 
NEWVAR11, _, _)) :: ( _, ( _, LEFTCURL1left, _)) :: rest671)) => let
 val  result = MlyValue.CommandSeq (fn _ => let val  (NEWVAR1 as 
NEWVAR11) = NEWVAR11 ()
 in (NEWVAR1)
end)
 in ( LrTable.NT 6, ( result, LEFTCURL1left, RIGHTCURL1right), rest671
)
end
|  ( 12, ( ( _, ( MlyValue.NEWVAR1 NEWVAR11, _, NEWVAR11right)) :: _
 :: ( _, ( MlyValue.Command Command1, Command1left, _)) :: rest671))
 => let val  result = MlyValue.NEWVAR1 (fn _ => let val  (Command as 
Command1) = Command1 ()
 val  (NEWVAR1 as NEWVAR11) = NEWVAR11 ()
 in (Command::NEWVAR1)
end)
 in ( LrTable.NT 11, ( result, Command1left, NEWVAR11right), rest671)

end
|  ( 13, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.Command 
Command1, Command1left, _)) :: rest671)) => let val  result = 
MlyValue.NEWVAR1 (fn _ => let val  (Command as Command1) = Command1 ()
 in (Command::nil)
end)
 in ( LrTable.NT 11, ( result, Command1left, SEMICOLON1right), rest671
)
end
|  ( 14, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: _ :: ( _, ( MlyValue.Variable Variable1, Variable1left, _)) :: 
rest671)) => let val  result = MlyValue.Command (fn _ => let val  (
Variable as Variable1) = Variable1 ()
 val  (Expression as Expression1) = Expression1 ()
 in (
if ((int_data_type_checker(VAREXP(Variable))+int_data_type_checker(Expression) = 2) orelse
                                        (int_data_type_checker(VAREXP(Variable))+int_data_type_checker(Expression) = 0)) 
                                        then exprcmd(Variable,Expression) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 7, ( result, Variable1left, Expression1right), 
rest671)
end
|  ( 15, ( ( _, ( MlyValue.Variable Variable1, _, Variable1right)) :: 
( _, ( _, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Variable as Variable1) = Variable1
 ()
 in (READ(Variable))
end)
 in ( LrTable.NT 7, ( result, READ1left, Variable1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: ( _, ( _, WRITE1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (
if (int_data_type_checker(Expression)=1) then WRITE(Expression) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 7, ( result, WRITE1left, Expression1right), rest671)

end
|  ( 17, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.CommandSeq 
CommandSeq2, _, _)) :: _ :: ( _, ( MlyValue.CommandSeq CommandSeq1, _,
 _)) :: _ :: ( _, ( MlyValue.Expression Expression1, _, _)) :: ( _, (
 _, IF1left, _)) :: rest671)) => let val  result = MlyValue.Command
 (fn _ => let val  (Expression as Expression1) = Expression1 ()
 val  CommandSeq1 = CommandSeq1 ()
 val  CommandSeq2 = CommandSeq2 ()
 in (
if (bool_data_type_checker(Expression) = 1) then ITE(Expression,CommandSeq1,CommandSeq2) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 7, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 18, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.CommandSeq 
CommandSeq1, _, _)) :: _ :: ( _, ( MlyValue.Expression Expression1, _,
 _)) :: ( _, ( _, WH1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in (
if (bool_data_type_checker(Expression) = 1) then WHILE(Expression,CommandSeq) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 7, ( result, WH1left, ENDWH1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if (int_data_type_checker(Expression1)*int_data_type_checker(Expression2) = 1) then PLUS(Expression1,Expression2) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 20, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if (int_data_type_checker(Expression1)*int_data_type_checker(Expression2) = 1) then MINUS(Expression1,Expression2) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if (bool_data_type_checker(Expression1)*bool_data_type_checker(Expression2) = 1) then OR(Expression1,Expression2) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 22, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if (int_data_type_checker(Expression1)*int_data_type_checker(Expression2) = 1) then TIMES(Expression1,Expression2) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 23, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if (int_data_type_checker(Expression1)*int_data_type_checker(Expression2) = 1) then DIV(Expression1,Expression2) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 24, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if (int_data_type_checker(Expression1)*int_data_type_checker(Expression2) = 1) then MOD(Expression1,Expression2) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 25, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if (bool_data_type_checker(Expression1)*bool_data_type_checker(Expression2) = 1) then AND(Expression1,Expression2) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 26, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.Expression (fn _ => let val  (NUM as 
NUM1) = NUM1 ()
 in (NUMEXP(NUM))
end)
 in ( LrTable.NT 8, ( result, NUM1left, NUM1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = MlyValue.Expression
 (fn _ => let val  (Variable as Variable1) = Variable1 ()
 in (VAREXP(Variable))
end)
 in ( LrTable.NT 8, ( result, Variable1left, Variable1right), rest671)

end
|  ( 28, ( ( _, ( _, _, RIGHTPAREN1right)) :: ( _, ( 
MlyValue.Expression Expression1, _, _)) :: ( _, ( _, LEFTPAREN1left, _
)) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  (Expression as Expression1) = Expression1 ()
 in ((Expression))
end)
 in ( LrTable.NT 8, ( result, LEFTPAREN1left, RIGHTPAREN1right), 
rest671)
end
|  ( 29, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: ( _, ( _, NEGATE1left, _)) :: rest671)) => let val  result = 
MlyValue.Expression (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (
if (int_data_type_checker(Expression)=1) then NEGATE(Expression) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 8, ( result, NEGATE1left, Expression1right), rest671)

end
|  ( 30, ( ( _, ( MlyValue.TRUE TRUE1, TRUE1left, TRUE1right)) :: 
rest671)) => let val  result = MlyValue.Expression (fn _ => let val  (
TRUE as TRUE1) = TRUE1 ()
 in (BOOLEXP(TRUE))
end)
 in ( LrTable.NT 8, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.FALSE FALSE1, FALSE1left, FALSE1right)) :: 
rest671)) => let val  result = MlyValue.Expression (fn _ => let val  (
FALSE as FALSE1) = FALSE1 ()
 in (BOOLEXP(FALSE))
end)
 in ( LrTable.NT 8, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.Comparison Comparison1, Comparison1left, 
Comparison1right)) :: rest671)) => let val  result = 
MlyValue.Expression (fn _ => let val  (Comparison as Comparison1) = 
Comparison1 ()
 in (COMPEXP(Comparison))
end)
 in ( LrTable.NT 8, ( result, Comparison1left, Comparison1right), 
rest671)
end
|  ( 33, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.Expression (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (
if (bool_data_type_checker(Expression)=1) then NOT(Expression) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 8, ( result, NOT1left, Expression1right), rest671)

end
|  ( 34, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Comparison (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 2) orelse
                                        (int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 0)) 
                                        then GT(Expression1,Expression2) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 9, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 35, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Comparison (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 2) orelse
                                        (int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 0)) 
                                        then LT(Expression1,Expression2) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 9, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 36, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Comparison (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 2) orelse
                                        (int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 0)) 
                                        then LEQ(Expression1,Expression2) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 9, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 37, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Comparison (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 2) orelse
                                        (int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 0)) 
                                        then GEQ(Expression1,Expression2) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 9, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 38, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Comparison (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 2) orelse
                                        (int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 0)) 
                                        then EQ(Expression1,Expression2) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 9, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 39, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Comparison (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 2) orelse
                                        (int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 0)) 
                                        then NEQ(Expression1,Expression2) else raise Invalid_dataType
)
end)
 in ( LrTable.NT 9, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 40, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.Variable (fn _ => let val  (ID as ID1) = 
ID1 ()
 in (VAR(ID))
end)
 in ( LrTable.NT 10, ( result, ID1left, ID1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.NUMBER NUMBER1, _, NUMBER1right)) :: ( _, (
 _, PLUS1left, _)) :: rest671)) => let val  result = MlyValue.NUM (fn
 _ => let val  (NUMBER as NUMBER1) = NUMBER1 ()
 in (POSITIVE(NUMBER))
end)
 in ( LrTable.NT 12, ( result, PLUS1left, NUMBER1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.NUMBER NUMBER1, NUMBER1left, NUMBER1right))
 :: rest671)) => let val  result = MlyValue.NUM (fn _ => let val  (
NUMBER as NUMBER1) = NUMBER1 ()
 in (NUMERICAL(NUMBER))
end)
 in ( LrTable.NT 12, ( result, NUMBER1left, NUMBER1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : While_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUMBER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUMBER (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun LEFTCURL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun RIGHTCURL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LEFTPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun RIGHTPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun SEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.TRUE (fn () => i),p1,p2))
fun FALSE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.FALSE (fn () => i),p1,p2))
fun PROG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun BOOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.BOOL (fn () => i),p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun WH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun SET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
end
end
