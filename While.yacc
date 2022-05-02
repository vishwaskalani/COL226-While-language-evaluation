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
%%

%pos int
%term ID of string| NUMBER of int| PLUS | TIMES | PRINT | MINUS | DIV | MOD | GT | LT 
        | GEQ | LEQ | EQ | NEQ | LEFTCURL | RIGHTCURL | LEFTPAREN | RIGHTPAREN | AND | OR
        | SEQ | TRUE of bool| FALSE of bool| PROG | VAR | INT of string| BOOL of string| READ | WRITE | IF | THEN | ELSE
        | ENDIF | WH | DO | ENDWH | COLON | COMMA | NEGATE | NOT | SEMICOLON | EOF | SET
%nonterm    Program of PROG| 
            Block of (((vari list)*types) list)*(cmd list)|
            DeclarationSeq of ((vari list)*types) list|
            Declaration of ((vari list)*types)|
            Type of types|
            VariableList of vari list|
            CommandSeq of cmd list|
            Command of cmd|
            Expression of expr|
            Comparison of comp|
            Variable of vari|
            NEWVAR1 of cmd list|
            NUM of numer

%right EQ NEQ
%right IF THEN ELSE ENDIF
%right WH DO ENDWH
%left OR 
%left AND 
%right NOT
%left GT LT GEQ LEQ
%left PLUS MINUS 
%left TIMES DIV MOD
%right NEGATE


%eop EOF
%noshift EOF
%nodefault
%name While
%start Program
%verbose
(* %keyword PROG VAR INT BOOL READ WRITE IF THEN ELSE ENDIF WH DO ENDWH *)
%arg (fileName) : string

%%
Program: PROG ID SEQ Block (PROG(ID,Block))
Block: DeclarationSeq CommandSeq ((DeclarationSeq,CommandSeq)) | CommandSeq (([],CommandSeq))
DeclarationSeq: Declaration DeclarationSeq (Declaration::DeclarationSeq)| Declaration (Declaration::nil)
Declaration: VAR VariableList COLON Type SEMICOLON (put_in_symbol_table(VariableList,Type,[]))
Type : INT (inter(INT))| BOOL (booler(BOOL))
VariableList : Variable COMMA VariableList (Variable::VariableList) | Variable (Variable::nil)
CommandSeq : LEFTCURL RIGHTCURL ([])| LEFTCURL NEWVAR1 RIGHTCURL (NEWVAR1)
NEWVAR1 : Command SEMICOLON NEWVAR1 (Command::NEWVAR1) | Command SEMICOLON (Command::nil)
Command :   Variable SET Expression (if ((int_data_type_checker(VAREXP(Variable))+int_data_type_checker(Expression) = 2) orelse
                                        (int_data_type_checker(VAREXP(Variable))+int_data_type_checker(Expression) = 0)) 
                                        then exprcmd(Variable,Expression) else raise Invalid_dataType)|
            READ Variable (READ(Variable))|
            WRITE Expression (if (int_data_type_checker(Expression)=1) then WRITE(Expression) else raise Invalid_dataType)|
            IF Expression THEN CommandSeq ELSE CommandSeq ENDIF (if (bool_data_type_checker(Expression) = 1) then ITE(Expression,CommandSeq1,CommandSeq2) else raise Invalid_dataType)|
            WH Expression DO CommandSeq ENDWH (if (bool_data_type_checker(Expression) = 1) then WHILE(Expression,CommandSeq) else raise Invalid_dataType)

Expression :    Expression PLUS Expression (if (int_data_type_checker(Expression1)*int_data_type_checker(Expression2) = 1) then PLUS(Expression1,Expression2) else raise Invalid_dataType)|
                Expression MINUS Expression (if (int_data_type_checker(Expression1)*int_data_type_checker(Expression2) = 1) then MINUS(Expression1,Expression2) else raise Invalid_dataType)|
                Expression OR Expression (if (bool_data_type_checker(Expression1)*bool_data_type_checker(Expression2) = 1) then OR(Expression1,Expression2) else raise Invalid_dataType)|
                Expression TIMES Expression (if (int_data_type_checker(Expression1)*int_data_type_checker(Expression2) = 1) then TIMES(Expression1,Expression2) else raise Invalid_dataType)|
                Expression DIV Expression (if (int_data_type_checker(Expression1)*int_data_type_checker(Expression2) = 1) then DIV(Expression1,Expression2) else raise Invalid_dataType)|
                Expression MOD Expression (if (int_data_type_checker(Expression1)*int_data_type_checker(Expression2) = 1) then MOD(Expression1,Expression2) else raise Invalid_dataType)|
                Expression AND Expression (if (bool_data_type_checker(Expression1)*bool_data_type_checker(Expression2) = 1) then AND(Expression1,Expression2) else raise Invalid_dataType)|
                NUM (NUMEXP(NUM)) | 
                Variable (VAREXP(Variable))|
                LEFTPAREN Expression RIGHTPAREN ((Expression))|
                NEGATE Expression(if (int_data_type_checker(Expression)=1) then NEGATE(Expression) else raise Invalid_dataType)|
                TRUE (BOOLEXP(TRUE))| 
                FALSE (BOOLEXP(FALSE))| 
                Comparison (COMPEXP(Comparison))|
                NOT Expression (if (bool_data_type_checker(Expression)=1) then NOT(Expression) else raise Invalid_dataType)

Comparison : Expression GT Expression (if ((int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 2) orelse
                                        (int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 0)) 
                                        then GT(Expression1,Expression2) else raise Invalid_dataType) |
             Expression LT Expression (if ((int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 2) orelse
                                        (int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 0)) 
                                        then LT(Expression1,Expression2) else raise Invalid_dataType) |
             Expression LEQ Expression (if ((int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 2) orelse
                                        (int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 0)) 
                                        then LEQ(Expression1,Expression2) else raise Invalid_dataType) |
             Expression GEQ Expression (if ((int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 2) orelse
                                        (int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 0)) 
                                        then GEQ(Expression1,Expression2) else raise Invalid_dataType) |
             Expression EQ Expression (if ((int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 2) orelse
                                        (int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 0)) 
                                        then EQ(Expression1,Expression2) else raise Invalid_dataType) |
             Expression NEQ Expression (if ((int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 2) orelse
                                        (int_data_type_checker(Expression1)+int_data_type_checker(Expression2) = 0)) 
                                        then NEQ(Expression1,Expression2) else raise Invalid_dataType)

Variable : ID (VAR(ID))
NUM : PLUS NUMBER(POSITIVE(NUMBER)) | NUMBER(NUMERICAL(NUMBER))
