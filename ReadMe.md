# Assignment 3
### Assignment description
---
In this assignment we need to execute the ASTs on a VMC machine by first transforming each program as a sequence of operations (either as tokens or as string representations of the operators or any other data-type that you find it convenient to convert them to) in post-fix form.

### Submission details
---

The zipped folder **2020CS10411.zip** contains the following files apart from README.md:-
- **While.lex** :: For lexing and token generation
- **While.yacc** :: For parsing and semantic analysis
- **While.lex.sml** :: Formed by ml-lex tool
- **While.yacc.sml** :: Formed by ml-yacc tool
- **While.yacc.sig** :: Formed by ml-yacc tool
- **While.yacc.desc** :: Formed by ml-yacc tool
- **While&#46;cm** :: A file to run all the files together
- **glue.sml** :: File to glue yacc and lex file
- **compiler.sml** :: File to give a tree structure from input program
- **while_ast.sml** :: Main file for building tree structure
- **testProg.While** :: Test programs in "While" language, one can put the programs to be tested in this file and follow the commands given in further section "How to run and build the AST" 
- **stack.sml** :: This contains implementation of a general-purpose functional stack structure 
- **VMCmachine.sml** :: This file implements the semantic rules of the VMC machine using the stack operations defined in the signature of the stacks of the VMC machine

### Type Checking
---
**Few changes have been made in the previously submitted files to typecheck the program**
- `Change in grammar :`The non terminals like IntExpression, BoolExpression, IntFactor, BoolFactor, IntTerm, BoolTerm have been merged into a single non terminal Expression. The precedence has been thus set in the yacc file. This removes the conflicts variable was facing in the derivation, whether from boolean side or from integer side. Further, type checking has been done while applying the production rules.
- `Lookup table :`While we are declaring the variables, we are storing the type of the variables in a `hashtable` which is an inbuilt structure in sml. Further while doing any operation on the variable, we are checking its type form the look up table.



### The rules for changes in VMC machine
---
- For Terminal tokens in the stack like numerical values, identifiers, boolean values. They have been pushed in the stack V along with the constructor used for them and stack C is popped. For instance the function rules returns this in such case:- 
```
(push(N(x),V),pop(C),M)
```
- For binary operators the top two elements of the stack V have been operated upon with the operation on the top of stack C for instance the rules does this for Plus operator (here the binary_evaluator will evaluate the result to be pushed in stack V):- 
```
(push(binary_evaluator(a1,a2,"Plus",M),V2),pop(C),M)
```
- For unary operators, an element is taken from the top of stack V, and is operated upon with the operation on the top of stack C for instance the rules does this for Negate operator (here the unary_evaluator will evaluate the result to be pushed in stack V):- 
```
(push(unary_evaluator(a1,"Negate",M),V1),pop(C),M)
```
- For `read` token, the input from user is taken and is fed to the memory location of the variable on the top of stack C:- 
```
(V1,pop(C),update_at_index(M,HashTable.lookup(indexer)(decode_v5(a1)),input_user))
```
- For `write` token, the content on the top of the stack V is printed to the console (in write a let statement is used for printing):- 
```
(V1,pop(C),M)
```
- For tokens like `ITE` and `While` which require us to do command sequence on conditional basis, have been implemented through looking some distance deep in stack and separating the boolean expressions and command sequences as per the rules given in assignment pdf.

### Design Decisions
---

1. **Design and datatype of Stacks V and C** : A new datatype has been defined in the file `while_ast.sml`, which encloses all the types of tokens we are going to transfer between the stacks. The type has been named `dynamic`, referring to the homogeneous nature of the stack. 
2. **Post fix expressions** : `postfix(x : cmd list)` is a function defined in the `VMCmachine.sml` which converts a command sequence into the corresponding postfix list. The list of postfix appears in a method opposite to usual imagination but is easy to implement for instance 
   ```
   2+3 => Plus 3 2
   ```
3. The stack C is initiated by first reversing this postfix list and then converting the list into stack.
4. Some rules to change the state of the VMC machine have been modified for the evaluation purpose. For instance some rules which are redundant `E.◦00`,`E.◦01`,`E.◦10` and `E.◦10` have not been defined explicitly in the `rules` function.
5.  Instead of pushing the value of an identifier, the rules function pushes the constructor of identifier itself, so that incase the token of `set` appears, then we can access the index of the variable and make necessary updates in the memory. 
6.  **Memory array and indexing** : For creating memory as an array, a function first calculates the total number of variables in the tree, and then memory is created of that size. The indexing in memory is done in sequential order of the declaration. The indexes of the variable are seen from a lookup table created in the file "VMCmachine.sml"
7.  **Stack Functions** : The toString function in the structure `Stack`, inserts a separator "." for a better look of printed stack.
8.  **Input and output types of key functions** :
- `rules : (dynamic Stack * dynamic Stack * int array) -> (dynamic Stack * dynamic Stack * int array)`
- `toString : (dynamic Stack * dynamic Stack * int array) -> string`
- `postfix(x : cmd list) : dynamic list`
- `execute(AST)` : This function takes the AST created by the yacc and lex files which is of type "AST.PROG" and then further evaluates the tree using the semantic rules written in the function rules. 


### Other Implementation Decisions
---
1. The V,M,C of the VMC machine have not been referenced to the Vmc Machine object created to avoid using features of `imperitive programming` in `functional programming`
2. Various helper functions which have been used in writing the rules function are kept outside the structure `Vmc`. 
3. The evaluation for pushing operated values in the `V` stack has been done using separate functions called `binary_evaluator` and `unary_evaluator`.
4. `Global Hashtable : indexer` in file `VMCmachine.sml`, `Global Hashtable : ht` in file `While.yacc`, the hashtable are kept global in files and have been updated gloabally throughout the files.
5. Printing the output of `Vmc.toString` function after each application of the function rules so that we can visualize how the evaluation is going on. The return type of execute however remains same as provided in the assignment pdf.


### Commands for Building an AST (part of previous assignment 3 as well)
---
1. In the terminal after loading `sml`, write the command :
`CM.make "While.cm";`
This command will compile all the files. 
2. Put the program to be parsed in the file `testProg.While`.
3. Put the following 3 commands one by one in terminal after compilation to view tree up to entire depths.
- `Control.Print.printLength := 10000;`
- `Control.Print.printDepth := 10000; `
- `Control.Print.stringDepth := 10000;`
4. Further to parse the test file and get the corresponding AST, put the following command in terminal : 
`While.compile "testProg.While";`
5. Then we will be able to see all the tokens lexed printed and the AST following that. The root node of the AST will be the `first non terminal datatype PROG`.


### Commands for Executing or evaluating the AST
---
1. In the terminal after loading `sml`, write the command :
`CM.make "While.cm";`
This command will compile all the files. 
2. Put the program to be executed in the file `testProg.While`.
3. Further to put the AST in a variable tr : 
`val tr = While.compile "testProg.While";`
4. Now we need to use the structure Stack in out functions so type in terminal `use "stack.sml";`
5. Now we need to use the `execute` function of the `VMCmachine.sml` file so type in terminal `use "VMCmachine.sml";`
6. Further to evaluate and show (V,C,M) in all stages of evaluation, type `execute(tr);` in terminal.
7. The commands thus to be executed in order are
```
CM.make "While.cm";
```
```
val tr = While.compile "testProg.While";
```
```
use "stack.sml";
```
```
use "VMCmachine.sml";
```
```
execute(tr);
```


### Acknowledgements
---
- `For assignment 3 :` I used **User’s Guide to ML-Lex and ML-Yacc** to understand basic features and syntax of ML-Lex, ML-Yacc and other pieces of code which link them. I have used the glue.sml, compiler.sml and .cm file to run all files together which is provided in the section **ML-Lex and ML-Yacc in a larger project** in concern with the example language **Pi**. 
I have also referred to the **Sample interactive calculator for ML-Yacc** provided on the site `http://www.smlnj.org/doc/ML-Yacc/`. I fully acknowledge all the authors of these great resources. Apart from these, the hypernotes have been useful in understanding the process of lexing, parsing and abstract tree construction.
- `For assignment 4 :` I referred to the documentation of sml on online resources to know the meaning of the stack functions (in the list documention) which have to be implemented.


