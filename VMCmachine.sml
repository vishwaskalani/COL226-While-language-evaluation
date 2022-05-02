open AST
open FunStack
exception invalidOperation;
exception IndexError;

(* This is the hashmap with index of variables in memory *)
val indexer : (string, int) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (100, Fail "not found");

(* This is a simple decoder for pattern matching *)
fun decode_v2(VAR(x)) = x;

(* This is a simple decoder for pattern matching *)
fun decode_v3((x:vari list,y:types)) = x;

(* This is a simple decoder for pattern matching *)
fun decode_v4(N(x)) = x|
    decode_v4(_) = raise invalidOperation;

(* This is a simple decoder for pattern matching *)
fun decode_v5(S(x)) = x|
    decode_v5(_) = raise invalidOperation;

(* Array element at any index *)
fun at_index_array(arr,i)=
    if i<0 then raise IndexError else
    Array.sub(arr,i);

(* This is a simple decoder for pattern matching *)
fun decode_v6(S(x),arr) = at_index_array(arr,HashTable.lookup(indexer)(x))|
    decode_v6(N(x),arr) = x |
    decode_v6(_,arr) = raise invalidOperation;

(* Reverses a list *)
fun reverse(l)=
    if length(l)=0 then l
    else reverse(tl(l))@[hd(l)];

(*This function returns the array updated at the ith element with value j*)
(*It raises error for negative index*)
fun update_at_index(a,i,j)=
    if i<0 then raise IndexError else
    let 
        val unit = Array.update(a,i,j)
    in
        a
    end;

(* Allocates index in hashtable*)
fun index_allotter(k,l:vari list)=
    if length(l)=0 then ()
    else
    let
        val a = HashTable.insert(indexer)(decode_v2(hd(l)),k)
    in
        index_allotter(k+1,tl(l))
    end;

(* Counts variable in vari list for memory array *)
fun variable_counter((x:vari list,y:types)):int= length(x);

(* Total variable in entire declaration - helper *)
fun total_variable_counter(l,m)=
    if length(l)=0 then 0 else
    let 
        val p = index_allotter(m,reverse(decode_v3(hd(l))))
    in
    variable_counter(hd(l))+total_variable_counter(tl(l),m+variable_counter(hd(l)))
    end;

(* Total variable in entire declaration *)
fun variables_in_tree(PROG(x,(ld,lc))) = total_variable_counter(ld,0);

(*This function gives an integer after taking input from the user*)
fun input_taker(m)=
    let
        val x=print("Enter the input for read command : "^"\n")
        val str = valOf (TextIO.inputLine TextIO.stdIn)
        val i : int = valOf (Int.fromString str)
    in
        i 
    end;

(* TREE *)
(* val tree = PROG
    ("testerProg",
     ([([VAR "C",VAR "B",VAR "A"],inter "int"),
       ([VAR "F",VAR "E"],booler "bool"),([VAR "N",VAR "M"],booler "bool")],
      [exprcmd (VAR "A",NUMEXP (NUMERICAL 5)),
       exprcmd (VAR "B",PLUS (NUMEXP (NUMERICAL 2),NUMEXP (POSITIVE 3))),
       exprcmd (VAR "C",MOD (VAREXP (VAR "B"),VAREXP (VAR "A")))])); *)

(* Converts an expression to postfix*)
fun postfix_expression(x : expr):dynamic list=
    case x of
    NUMEXP(NUMERICAL(y)) => [N y]|
    NUMEXP(POSITIVE(y)) => [N y]|
    VAREXP(VAR(y)) => [S y]|
    BOOLEXP(y) => [B y]|
    PLUS(y,z) => (Plus_Tok)::(postfix_expression(z)@postfix_expression(y))|
    MINUS(y,z) => (Minus_Tok)::(postfix_expression(z)@postfix_expression(y))|
    TIMES(y,z) => (Times_Tok)::(postfix_expression(z)@postfix_expression(y))|
    DIV(y,z) => (Div_Tok)::(postfix_expression(z)@postfix_expression(y))|
    MOD(y,z) => (Mod_Tok)::(postfix_expression(z)@postfix_expression(y))|
    OR(y,z) => (Or_Tok)::(postfix_expression(z)@postfix_expression(y))|
    AND(y,z) => (And_Tok)::(postfix_expression(z)@postfix_expression(y))|
    NEGATE(y) => (Negate_Tok)::(postfix_expression(y))|
    NOT(y) => (Not_Tok)::(postfix_expression(y))|
    COMPEXP(GT(y,z)) => (Gt_Tok)::(postfix_expression(z)@postfix_expression(y))|
    COMPEXP(LT(y,z)) => (Lt_Tok)::(postfix_expression(z)@postfix_expression(y))|
    COMPEXP(LEQ(y,z)) => (Leq_Tok)::(postfix_expression(z)@postfix_expression(y))|
    COMPEXP(GEQ(y,z)) => (Geq_Tok)::(postfix_expression(z)@postfix_expression(y))|
    COMPEXP(EQ(y,z)) => (Eq_Tok)::(postfix_expression(z)@postfix_expression(y))|
    COMPEXP(NEQ(y,z)) => (Neq_Tok)::(postfix_expression(z)@postfix_expression(y));

(* Converts a command in post fix *)
fun postfix_command(x : cmd):dynamic list=
    let 
        fun postfix_command_list(x : cmd list):dynamic list=
            if length(x)=0 then []
            else (Seq_Tok)::(postfix_command_list(tl(x))@postfix_command(hd(x)))
    in
    case x of
    exprcmd(y,z) => (Set_Tok)::(postfix_expression(z)@postfix_expression(VAREXP(y)))|
    READ(y)=>(Read_Tok)::postfix_expression(VAREXP(y))|
    WRITE(y)=>(Write_Tok)::postfix_expression(y)|
    ITE(m,n,p) => [Ite_Tok]@postfix_command_list(p)@[Lamda2]@postfix_command_list(n)@[Lamda1]@postfix_expression(m)|
    WHILE(m,n) => (While_Tok)::(postfix_command_list(n)@[LamdaWhile2]@postfix_expression(m))@[LamdaWhile1]
    end;


(* Post fix of command Seq *)
fun postfix(x : cmd list):dynamic list=
        if length(x)=0 then []
        else (Seq_Tok)::(postfix(tl(x))@postfix_command(hd(x)));
(* reverse(postfix(a)); *)

(* These are helper functions to evaluate if-else and while *)
fun sublist(l,i,j)=
    if i>j then []
    else List.drop(List.take(l,j),i);

fun helperHelp1(l,a,b,k)=
    if a>b then k 
    else if hd(l) = Lamda1 then helperHelp1(tl(l),a,b+1,k+1)
    else if hd(l) = Lamda2 then helperHelp1(tl(l),a+1,b,k+1)
    else helperHelp1(tl(l),a,b,k+1);

fun helperHelp2(l,a,b,k)=
    if a>b then k 
    else if hd(l) = Lamda1 then helperHelp2(tl(l),a,b+1,k+1)
    else if hd(l) = Ite_Tok then helperHelp2(tl(l),a+1,b,k+1)
    else helperHelp2(tl(l),a,b,k+1);

fun helperHelp3(l,a,b,k)=
    if a>b then k 
    else if hd(l) = LamdaWhile1 then helperHelp3(tl(l),a,b+1,k+1)
    else if hd(l) = LamdaWhile2 then helperHelp3(tl(l),a+1,b,k+1)
    else helperHelp3(tl(l),a,b,k+1);

fun helperHelp4(l,a,b,k)=
    if a>b then k 
    else if hd(l) = LamdaWhile1 then helperHelp4(tl(l),a,b+1,k+1)
    else if hd(l) = While_Tok then helperHelp4(tl(l),a+1,b,k+1)
    else helperHelp4(tl(l),a,b,k+1);

fun helperHelp5(l,k,k1)=
    if k<k1 then helperHelp5(tl(l),k+1,k1)
    else if hd(l)=LamdaWhile1 then k else
    helperHelp5(tl(l),k+1,k1);

fun helper(bExp,V,C,M)= 
    let 
        val stackList = FunStack.stack2list(C);
        val ind1 = helperHelp1(stackList,0,0,0)-1;
        val ind2 = helperHelp2(stackList,0,0,0)-1;
        val List_new_1 = List.drop(stackList,ind2+1);
        val sublist1 = sublist(stackList,0,ind1);
        val sublist2 = sublist(stackList,ind1+1,ind2);
    in
    if (bExp = 1) then 
        (V,list2stack(sublist1@List_new_1),M)
    else
        (V,list2stack(sublist2@List_new_1),M)
    end;

fun helperWhile2(bExp,V,C,M)= 
    let 
        val stackList = FunStack.stack2list(C);
        val stackList2= FunStack.stack2list(V);
        val ind1 = helperHelp3(stackList2,0,0,0)-1;
        val ind2 = helperHelp5(stackList2,0,ind1);
        val sublist1 = sublist(stackList2,0,ind1);
        val sublist2 = sublist(stackList2,ind1+1,ind2);
        val List3 = List.drop(stackList2,ind2+1);
    in
    if (bExp = 0) then 
        (list2stack(List3),C,M)
    else
        (list2stack(List3),list2stack(sublist1@[LamdaWhile1]@sublist2@[LamdaWhile2]@sublist1@[While_Tok]@stackList),M)
    end;

fun helperWhile(V,C,M)= 
    let 
        val stackList = FunStack.stack2list(C);
        val stackList2= FunStack.stack2list(V);
        val ind1 = helperHelp3(stackList,0,0,0)-1;
        val ind2 = helperHelp4(stackList,0,0,0)-1;
        val List_new_1 = List.drop(stackList,ind2);
        val sublist1 = sublist(stackList,0,ind1);
        val sublist2 = sublist(stackList,ind1+1,ind2);
    in
    (list2stack(sublist2@[LamdaWhile2]@sublist1@[LamdaWhile1]@stackList2),list2stack(sublist1@List_new_1),M)
    end;


(* Applies binary operators *)
fun binary_evaluator(y,x,operation,arr)=
    case x of
    N(a) =>
        (case y of
        N(b) => (case operation of
                "Plus" => N(a+b)|
                "Minus" => N(a-b)|
                "Times" => N(a*b)|
                "Div" => N(a div b)|
                "Mod" => N(a mod b) |
                "And" => N(a*b) |
                "Or" => N((a+b+1) div 2) |
                "Gt" => if a>b then N(1) else N(0)|
                "Lt" => if a<b then N(1) else N(0)|
                "Geq" => if a>=b then N(1) else N(0)|
                "Leq" => if a<=b then N(1) else N(0)|
                "Eq" => if a=b then N(1) else N(0)|
                "Neq" => if a<>b then N(1) else N(0)|
                _ => raise invalidOperation
                )|
        S(b) => (case operation of
                "Plus" => N(a+at_index_array(arr,HashTable.lookup(indexer)(b)))|
                "Minus" => N(a-at_index_array(arr,HashTable.lookup(indexer)(b)))|
                "Times" => N(a*at_index_array(arr,HashTable.lookup(indexer)(b)))|
                "Div" => N(a div at_index_array(arr,HashTable.lookup(indexer)(b)))|
                "Mod" => N(a mod at_index_array(arr,HashTable.lookup(indexer)(b))) |
                "And" => N(a*at_index_array(arr,HashTable.lookup(indexer)(b))) |
                "Or" => N((a+at_index_array(arr,HashTable.lookup(indexer)(b))+1) div 2) |
                "Gt" => if a>at_index_array(arr,HashTable.lookup(indexer)(b)) then N(1) else N(0)|
                "Lt" => if a<at_index_array(arr,HashTable.lookup(indexer)(b)) then N(1) else N(0)|
                "Geq" => if a>=at_index_array(arr,HashTable.lookup(indexer)(b)) then N(1) else N(0)|
                "Leq" => if a<=at_index_array(arr,HashTable.lookup(indexer)(b)) then N(1) else N(0)|
                "Eq" => if a=at_index_array(arr,HashTable.lookup(indexer)(b)) then N(1) else N(0)|
                "Neq" => if a<>at_index_array(arr,HashTable.lookup(indexer)(b)) then N(1) else N(0)|
                _ => raise invalidOperation
                )|
        _ => raise invalidOperation)|
    S(a) =>
        (case y of
        N(b) => (case operation of
                "Plus" => N(at_index_array(arr,HashTable.lookup(indexer)(a))+b)|
                "Minus" => N(at_index_array(arr,HashTable.lookup(indexer)(a))-b)|
                "Times" => N(at_index_array(arr,HashTable.lookup(indexer)(a))*b)|
                "Div" => N(at_index_array(arr,HashTable.lookup(indexer)(a)) div b)|
                "Mod" => N(at_index_array(arr,HashTable.lookup(indexer)(a)) mod b) |
                "And" => N(at_index_array(arr,HashTable.lookup(indexer)(a))*b) |
                "Or" => N((at_index_array(arr,HashTable.lookup(indexer)(a))+b+1) div 2) | 
                "Gt" => if at_index_array(arr,HashTable.lookup(indexer)(a))>b then N(1) else N(0)|
                "Lt" => if at_index_array(arr,HashTable.lookup(indexer)(a))<b then N(1) else N(0)|
                "Geq" => if at_index_array(arr,HashTable.lookup(indexer)(a))>=b then N(1) else N(0)|
                "Leq" => if at_index_array(arr,HashTable.lookup(indexer)(a))<=b then N(1) else N(0)|
                "Eq" => if at_index_array(arr,HashTable.lookup(indexer)(a))=b then N(1) else N(0)|
                "Neq" => if at_index_array(arr,HashTable.lookup(indexer)(a))<>b then N(1) else N(0)|               
                _ => raise invalidOperation
                )|
        S(b) => (case operation of
                "Plus" => N(at_index_array(arr,HashTable.lookup(indexer)(a))+at_index_array(arr,HashTable.lookup(indexer)(b)))|
                "Minus" => N(at_index_array(arr,HashTable.lookup(indexer)(a))-at_index_array(arr,HashTable.lookup(indexer)(b)))|
                "Times" => N(at_index_array(arr,HashTable.lookup(indexer)(a))*at_index_array(arr,HashTable.lookup(indexer)(b)))|
                "Div" => N(at_index_array(arr,HashTable.lookup(indexer)(a)) div at_index_array(arr,HashTable.lookup(indexer)(b)))|
                "Mod" => N(at_index_array(arr,HashTable.lookup(indexer)(a)) mod at_index_array(arr,HashTable.lookup(indexer)(b))) |
                "And" => N(at_index_array(arr,HashTable.lookup(indexer)(a))*at_index_array(arr,HashTable.lookup(indexer)(b))) |
                "Or" => N((at_index_array(arr,HashTable.lookup(indexer)(a))+at_index_array(arr,HashTable.lookup(indexer)(b))+1) div 2) |
                "Gt" => if at_index_array(arr,HashTable.lookup(indexer)(a))>at_index_array(arr,HashTable.lookup(indexer)(b)) then N(1) else N(0)|
                "Lt" => if at_index_array(arr,HashTable.lookup(indexer)(a))<at_index_array(arr,HashTable.lookup(indexer)(b)) then N(1) else N(0)|
                "Geq" => if at_index_array(arr,HashTable.lookup(indexer)(a))>=at_index_array(arr,HashTable.lookup(indexer)(b)) then N(1) else N(0)|
                "Leq" => if at_index_array(arr,HashTable.lookup(indexer)(a))<=at_index_array(arr,HashTable.lookup(indexer)(b)) then N(1) else N(0)|
                "Eq" => if at_index_array(arr,HashTable.lookup(indexer)(a))=at_index_array(arr,HashTable.lookup(indexer)(b)) then N(1) else N(0)|
                "Neq" => if at_index_array(arr,HashTable.lookup(indexer)(a))<>at_index_array(arr,HashTable.lookup(indexer)(b)) then N(1) else N(0)|
                _ => raise invalidOperation
                )|
        _ => raise invalidOperation)|
    _ => raise invalidOperation;

(* Applies unary operators *)
fun unary_evaluator(x,operation,arr)=
    case x of
    N(a) =>
        (case operation of
        "Negate" => N(~1*a)|
        "Not" => N(1-a)|
        _ => raise invalidOperation
        )|
    S(a) =>
        (case operation of
        "Negate" => N(~1*at_index_array(arr,HashTable.lookup(indexer)(a)))|
        "Not" => N(1-at_index_array(arr,HashTable.lookup(indexer)(a)))|                
        _ => raise invalidOperation
        )|
    _ => raise invalidOperation;


(* This functions converts an array to string *)
fun array_to_string(arr,i)=
    if Array.length(arr) = 0 then ""
    else if i = Array.length(arr) then ""
    else Int.toString(at_index_array(arr,i))^"."^array_to_string(arr,i+1); 

(* This function converts the data type in stack to list *)
fun stringer(x)=
    case x of
    N (y) => Int.toString(y)
    | Plus_Tok => "Plus"
    | Minus_Tok => "Minus"
    | Times_Tok => "Times"
    | Div_Tok => "Div"
    | Mod_Tok => "Mod"
    | Set_Tok => "Set"
    | And_Tok => "And"
    | Or_Tok => "Or"
    | Negate_Tok => "Negate"
    | Not_Tok => "Not"
    | Seq_Tok => "Seq"
    | Read_Tok => "Read"
    | Write_Tok => "Write"
    | Geq_Tok => "Geq"
    | Leq_Tok => "Leq"
    | Gt_Tok => "Gt"
    | Lt_Tok => "Lt"
    | Eq_Tok => "Eq"
    | Neq_Tok => "Neq"
    | Ite_Tok => "Ite"
    | While_Tok=> "while"
    | B(true) => "true"
    | B(false) => "false"
    | S(y) => y
    | _ => "";



signature VMC =
sig
    val V : (dynamic Stack)
    val C : (dynamic Stack)
    val M : (int array)
    val rules : (dynamic Stack * dynamic Stack * int array) -> (dynamic Stack * dynamic Stack * int array)
    val toString : (dynamic Stack * dynamic Stack * int array) -> string
end

structure Vmc :> VMC =
struct
    val V : (dynamic Stack) =  (FunStack.create)
    val C : (dynamic Stack) =  (FunStack.create)
    val M : (int array ) =  (Array.array(0,0))   
    fun rules(V :dynamic Stack,C : dynamic Stack ,M : int array )=
    case top(C) of 
    N(x) => (push(N(x),V),pop(C),M)|
    B(true) => (push(N(1),V),pop(C),M)|
    B(false) => (push(N(0),V),pop(C),M)|
    S(x) => (push(S(x),V),pop(C),M)|
    Plus_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
                val a2 = top(V1)
                val V2 = pop(V1)
            in
                (push(binary_evaluator(a1,a2,"Plus",M),V2),pop(C),M)
            end|
    Minus_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
                val a2 = top(V1)
                val V2 = pop(V1)
            in
                (push(binary_evaluator(a1,a2,"Minus",M),V2),pop(C),M)
            end|
    Times_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
                val a2 = top(V1)
                val V2 = pop(V1)
            in
                (push(binary_evaluator(a1,a2,"Times",M),V2),pop(C),M)
            end|
    Div_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
                val a2 = top(V1)
                val V2 = pop(V1)
            in
                (push(binary_evaluator(a1,a2,"Div",M),V2),pop(C),M)
            end|
    Mod_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
                val a2 = top(V1)
                val V2 = pop(V1)
            in
                (push(binary_evaluator(a1,a2,"Mod",M),V2),pop(C),M)
            end|
    Gt_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
                val a2 = top(V1)
                val V2 = pop(V1)
            in
                (push(binary_evaluator(a1,a2,"Gt",M),V2),pop(C),M)
            end|
    Lt_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
                val a2 = top(V1)
                val V2 = pop(V1)
            in
                (push(binary_evaluator(a1,a2,"Lt",M),V2),pop(C),M)
            end|
    Geq_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
                val a2 = top(V1)
                val V2 = pop(V1)
            in
                (push(binary_evaluator(a1,a2,"Geq",M),V2),pop(C),M)
            end|
    Leq_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
                val a2 = top(V1)
                val V2 = pop(V1)
            in
                (push(binary_evaluator(a1,a2,"Leq",M),V2),pop(C),M)
            end|
    Eq_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
                val a2 = top(V1)
                val V2 = pop(V1)
            in
                (push(binary_evaluator(a1,a2,"Eq",M),V2),pop(C),M)
            end|
    Neq_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
                val a2 = top(V1)
                val V2 = pop(V1)
            in
                (push(binary_evaluator(a1,a2,"Neq",M),V2),pop(C),M)
            end|
    Or_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
                val a2 = top(V1)
                val V2 = pop(V1)
            in
                (push(binary_evaluator(a1,a2,"Or",M),V2),pop(C),M)
            end|
    And_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
                val a2 = top(V1)
                val V2 = pop(V1)
            in
                (push(binary_evaluator(a1,a2,"And",M),V2),pop(C),M)
            end|
    Set_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
                val a2 = top(V1)
                val V2 = pop(V1)
            in
                (V2,pop(C),update_at_index(M,HashTable.lookup(indexer)(decode_v5(a2)),decode_v4(a1)))
            end|
    Negate_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
            in
                (push(unary_evaluator(a1,"Negate",M),V1),pop(C),M)
            end|
    Not_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
            in
                (push(unary_evaluator(a1,"Not",M),V1),pop(C),M)
            end|
    Read_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
                val input_user = input_taker();
            in
                (V1,pop(C),update_at_index(M,HashTable.lookup(indexer)(decode_v5(a1)),input_user))
            end|
    Write_Tok  =>
            let
                val a1 = top(V)
                val V1 = pop(V)
                val write = print(Int.toString(decode_v6(a1,M))^"\n")
            in
                (V1,pop(C),M)
            end|
    Seq_Tok  => (V,pop(C),M)|
    Lamda1 => 
            let
                val bExp = decode_v6(top(V),M)
            in
                helper(bExp,pop(V),pop(C),M)
            end|
    LamdaWhile1 => 
                helperWhile(V,pop(C),M)|
    While_Tok=> 
            let
                val bExp = decode_v6(top(V),M)
            in
                helperWhile2(bExp,pop(V),pop(C),M)
            end|
    _ => (V,C,M);

    fun toString(V :dynamic Stack,C : dynamic Stack ,M : int array )=
        let
            val s1 = FunStack.toString stringer V
            val s2 = FunStack.toString stringer C
        in
            "Value Stack is [ "^s1^" ] ,Control Stack is [ "^s2^" ] ,Memory is [ "^array_to_string(M,0)^" ]"
        end;



end

fun execute_help(V,M,C) = 
    if empty(C)=true then (V,C,M) else
    let
    val (V,C,M) = Vmc.rules(V,C,M) 
    val x = print(Vmc.toString(V,C,M))
    val y = print("\n")
    val y = print("\n")
    in
    execute_help(V,M,C)
    end;



fun execute(PROG(x,(ld,lc)))=
    let
        val len = variables_in_tree(PROG(x,(ld,lc)));
        val a_mem = Array.array(len,0);
        val p_list = postfix(lc);
        val p_list = reverse(p_list);
        val V : (dynamic Stack) = FunStack.create;
        val C : (dynamic Stack) = list2stack(p_list);
        val M : (int array) = a_mem;
    in
        execute_help(V,M,C)             
    end;

(* val (Va,Ca,Ma) = execute(tree); *)


















    