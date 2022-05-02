signature STACK =
sig
    type 'a Stack
    exception EmptyStack
    exception Error of string
    val create: 'a Stack
    val push : 'a * 'a Stack -> 'a Stack
    val pop : 'a Stack -> 'a Stack
    val top : 'a Stack -> 'a
    val empty: 'a Stack -> bool
    val poptop : 'a Stack -> ('a * 'a Stack) option
    val nth : 'a Stack * int -> 'a
    val drop : 'a Stack * int -> 'a Stack
    val depth : 'a Stack -> int
    val app : ('a -> unit) -> 'a Stack -> unit
    val map : ('a -> 'b) -> 'a Stack -> 'b Stack
    val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
    val find : ('a -> bool) -> 'a Stack -> 'a option
    val filter : ('a -> bool) -> 'a Stack -> 'a Stack
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val exists : ('a -> bool) -> 'a Stack -> bool
    val all : ('a -> bool) -> 'a Stack -> bool
    val list2stack : 'a list -> 'a Stack (* Convert a list into a Stack *)
    val stack2list: 'a Stack -> 'a list (* Convert a Stack into a list *)
    val toString: ('a -> string) -> 'a Stack -> string
end

structure FunStack :> STACK =
    struct
    type 'a Stack = 'a list
    exception EmptyStack
    exception Error of string
    val create : 'a Stack = []
    fun empty (l : 'a list): bool =
        case l of
        [] => true|
        _ => false
    fun push (x : 'a, l : 'a Stack):'a Stack = x::l 
    fun pop (l : 'a Stack):'a Stack = 
        case l of
        [] => raise EmptyStack|
        (x::xt) => xt
    fun top (l : 'a Stack):'a =
        case l of
        [] => raise EmptyStack |
        (x::xt) => x
    fun poptop (l : 'a Stack):(('a * 'a Stack) option) =
        case l of
        [] => NONE |
        (x::xt) => SOME((x,xt))
    fun nth (l: 'a Stack,x : int):'a =
        case l of
        [] => raise EmptyStack |
        (h::xt) =>
            (case x of
            0 => h|
            _ => nth(xt,x-1))
    fun drop (l : 'a Stack,x : int):'a Stack =
        case x of
        0 => l|
        _ => 
            (case l of
            [] => raise EmptyStack |
            (h::xt) => drop(xt,x-1)) 
    fun depth (l : 'a Stack) : int =
        case l of
        [] => 0|
        (x::xt) => 1+depth(xt)
    fun app (f : 'a -> unit) (l : 'a Stack) : unit =
        case l of
        [] => ()|
        (x::xt) =>
            let
                val y = f(x)
            in
                app f xt
            end
    fun map (f : 'a -> 'b) (l : 'a Stack) :'b Stack =
        case l of
        [] => []|
        (x::xt) => f(x)::(map f xt)
    fun mapPartial (f : 'a -> 'b option) (l : 'a Stack) :'b Stack =
        case l of
        [] => []|
        (x::xt) =>
            (case f x of
            NONE => (mapPartial f xt) |
            SOME v =>  v::(mapPartial f xt))
    fun find (f : 'a -> bool) (l : 'a Stack) : 'a option=
        case l of
        [] => NONE|
        (x::xt) =>
            (case f(x) of
            true => SOME(x)|
            false => (find f xt))
    fun filter (f : 'a -> bool) (l : 'a Stack) : 'a Stack=
        case l of
        [] => []|
        (x::xt) =>
            (case f(x) of
            true => x::(filter f xt) |
            false =>(filter f xt) )    
    fun foldr (f : 'a * 'b -> 'b) (e : 'b) (l : 'a Stack) :'b =
        case l of
        [] => e |
        (x::xt) => f (x, foldr f e xt)
    fun foldl (f : 'a * 'b -> 'b) (e : 'b) (l : 'a Stack) :'b =
        case l of
        [] => e |
        (x::xt) => foldl f (f (x, e)) xt;
    fun exists (f : 'a -> bool) (l : 'a Stack) : bool=
        case l of
        [] => false|
        (x::xt) =>
            (case f(x) of
            true => true|
            false => (exists f xt))
    fun all (f : 'a -> bool) (l : 'a Stack) : bool=
        case l of
        [] => true|
        (x::xt) =>
            (case f(x) of
            false => false|
            true => (all f xt))
    fun list2stack (l : 'a list) : 'a Stack =
        case l of
        [] => create |
        (x::xt) => push(x,list2stack(xt))
    fun stack2list (l : 'a Stack) : 'a list = 
        case l of
        [] => [] |
        (x::xt) => x::stack2list(xt)

    fun toString (a2s : 'a -> string) (l : 'a Stack ) : string =
        case l of
        [] => "" |
        (x::xt) => a2s(x)^"."^(toString a2s xt)
end
