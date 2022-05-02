(* glue.sml Create a lexer and a parser *)
structure WhileLrVals = WhileLrValsFun(
    structure Token = LrParser.Token);
structure WhileLex = WhileLexFun(
    structure Tokens = WhileLrVals.Tokens);
structure WhileParser = JoinWithArg(
    structure ParserData = WhileLrVals.ParserData
    structure Lex=WhileLex
    structure LrParser=LrParser);