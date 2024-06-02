type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

datatype error_info = UnknownCharacter of char

exception LexerError of error_info * pos

val currLine = ref 1
fun currChar s = String.sub (s, (size s) - 1)
fun incr v = v := !v + 1
fun resetState () = (
    currLine := 1
)
fun raiseError info linenum = (resetState(); raise LexerError (info, linenum))

fun eof () = let
    val finalLine = !currLine
in
    resetState();
    Tokens.EOF (finalLine, finalLine)
end

%%

%header (functor GCLLexerFun(structure Tokens : GCL_TOKENS));

%%

[\ \t\r]+ => (lex());
"\n" => (incr currLine; lex());

"+" => (Tokens.PLUS (!currLine, !currLine));
"-" => (Tokens.MINUS (!currLine, !currLine));
"*" => (Tokens.TIMES (!currLine, !currLine));
"/" => (Tokens.DIVIDE (!currLine, !currLine));
"=" => (Tokens.EQUAL (!currLine, !currLine));
"!=" => (Tokens.NOT_EQUAL (!currLine, !currLine));
"and" => (Tokens.AND (!currLine, !currLine));
"or" => (Tokens.OR (!currLine, !currLine));
":=" => (Tokens.ASSIGN (!currLine, !currLine));
";" => (Tokens.SEMICOLON (!currLine, !currLine));
"," => (Tokens.COMMA (!currLine, !currLine));
"|" => (Tokens.BAR (!currLine, !currLine));
"->" => (Tokens.ARROW (!currLine, !currLine));
"(" => (Tokens.LPAREN (!currLine, !currLine));
")" => (Tokens.RPAREN (!currLine, !currLine));

[0-9]+ => (Tokens.INT (valOf (Int.fromString (yytext)), !currLine, !currLine));
"true" => (Tokens.BOOL (true, !currLine, !currLine));
"false" => (Tokens.BOOL (false, !currLine, !currLine));
"skip" => (Tokens.SKIP (!currLine, !currLine));
"abort" => (Tokens.ABORT (!currLine, !currLine));
"if" => (Tokens.IF (!currLine, !currLine));
"end" => (Tokens.END (!currLine, !currLine));
"block" => (Tokens.BLOCK (!currLine, !currLine));

[a-zA-Z][a-zA-Z_0-9]* => (Tokens.IDENT (yytext, !currLine, !currLine));

. => (raiseError (UnknownCharacter (currChar yytext)) (!currLine));