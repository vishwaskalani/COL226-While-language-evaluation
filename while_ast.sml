(* structure AST=
    struct
    type ident = string
    type number = int
    datatype addop = Plus | Minus
    datatype multop = Times | Div | Mod
    datatype relop = Geq | Leq | Gt | Lt | Eq | Neq
    datatype program = program of ident*blk
            and blk = dcblk of dseq*cseq | cblk of cseq
            and dseq = dseqdecl of decl | recdseq of decl*dseq
            and decl = decl of vlist*types
            and types = inter of string | booler of string
            and vlist = varvlist of vari*vlist | termvlist of vari
            and vari = idvar of ident
            and cseq = emptycseq | reccseq of newtyp
            and newtyp = cmdnewtyp of cmd*newtyp | termcmd of cmd
            and cmd = exprcmd of vari*expr | readcmd of vari | writecmd of intexpr | itecmd of boolexpr*cseq*cseq| WHILE of boolexpr*cseq
            and expr = iexpr of intexpr | bexpr of boolexpr
            and intexpr = tintexpr of intterm | recintexpr of intexpr*addop*intterm
            and intterm = fintterm of intfact | recintterm of intterm*multop*intfact
            and intfact =numerintfact of numer | vintfact of vari|bintfact of intexpr|negintfact of intfact
            and boolexpr = tboolexpr of boolterm | recboolexpr of boolexpr*boolterm
            and boolterm = fboolfact of boolfact | recboolterm of boolterm*boolfact
            and boolfact = boolboolfact of bool|varboolfact of vari | compboolfact of comp | eboolfact of boolexpr| nboolfact of boolfact
            and comp = exprcomp of intexpr*relop*intexpr
            and numer = posnumber of number | termnumber of number
        
end; *)
(* 
structure AST=
    struct
    type ident = string
    type number = int
    datatype addop = Plus | Minus
    datatype multop = Times | Div | Mod
    datatype relop = Geq | Leq | Gt | Lt | Eq | Neq
    datatype program = program of ident*blk
            and blk = dcblk of (decl list)*cmd list | cblk of cmd list
            and decl = decl of (vari list)*types
            and types = inter of string | booler of string
            and vari = idvar of ident
            and cmd = exprcmd of vari*expr | readcmd of vari | writecmd of intexpr | itecmd of boolexpr*cmd list*cmd list| WHILE of boolexpr*cmd list
            and expr = iexpr of intexpr | bexpr of boolexpr
            and intexpr = tintexpr of intterm | recintexpr of intexpr*addop*intterm
            and intterm = fintterm of intfact | recintterm of intterm*multop*intfact
            and intfact =numerintfact of numer | vintfact of vari|bintfact of intexpr|negintfact of intfact
            and boolexpr = tboolexpr of boolterm | recboolexpr of boolexpr*boolterm
            and boolterm = fboolfact of boolfact | recboolterm of boolterm*boolfact
            and boolfact = boolboolfact of bool|varboolfact of vari | compboolfact of comp | eboolfact of boolexpr| nboolfact of boolfact
            and comp = exprcomp of intexpr*relop*intexpr
            and numer = posnumber of number | termnumber of number
        
end; *)


structure AST=
    struct
    type ident = string
    type number = int
    datatype read_write = Read|Write
    datatype if_while = Ite|Wh
    datatype rel_operation = Geq | Leq | Gt | Lt | Eq | Neq
    datatype PROG = PROG of ident*((((vari list)*types) list)*(cmd list))
            and types = inter of string | booler of string
            and vari = VAR of ident
            and cmd = exprcmd of vari*expr | READ of vari | WRITE of expr | ITE of expr*cmd list*cmd list| WHILE of expr*cmd list
            and expr = PLUS of expr*expr |
                        MINUS of expr*expr |
                        TIMES of expr*expr |
                        DIV of expr*expr |
                        MOD of expr*expr |
                        NUMEXP of numer| 
                        VAREXP of vari|
                        NEGATE of expr|
                        OR of expr*expr | 
                        AND of expr*expr | 
                        NOT of expr| 
                        BOOLEXP of bool| 
                        COMPEXP of comp
            and comp = GT of expr*expr|
                       LT of expr*expr|
                       LEQ of expr*expr|
                       GEQ of expr*expr|
                       EQ of expr*expr|
                       NEQ of expr*expr 

            and numer = POSITIVE of number | NUMERICAL of number
        datatype dynamic = N of number
                 | Plus_Tok
                 | Minus_Tok
                 | Times_Tok
                 | Div_Tok
                 | Mod_Tok
                 | Set_Tok
                 | And_Tok
                 | Or_Tok
                 | Negate_Tok
                 | Not_Tok
                 | Seq_Tok
                 | Read_Tok
                 | Write_Tok
                 | Geq_Tok
                 | Leq_Tok
                 | Gt_Tok
                 | Lt_Tok
                 | Eq_Tok
                 | Neq_Tok
                 | Ite_Tok
                 | While_Tok
                 | B of bool
                 | S of string 
                 | Lamda1
                 | Lamda2
                 | LamdaWhile1
                 | LamdaWhile2
        
end;