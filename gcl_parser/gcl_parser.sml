structure GCLParser = struct

structure ParserLrVals = GCLLrValsFun( structure Token = LrParser.Token )
structure Lexer = GCLLexerFun( structure Tokens = ParserLrVals.Tokens )
structure Parser = Join( structure ParserData = ParserLrVals.ParserData
                         structure Lex = Lexer
                         structure LrParser = LrParser )
open Lexer.UserDeclarations
open Parser

end (* structure GCLParser *)