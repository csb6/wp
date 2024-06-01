signature TOKENS = sig
    type linenum = int
    type token

    val PLUS : linenum * linenum -> token
    val MINUS : linenum * linenum -> token
    val TIMES : linenum * linenum -> token
    val DIVIDE : linenum * linenum -> token
    val EQUAL : linenum * linenum -> token
    val NOT_EQUAL : linenum * linenum -> token
    val AND : linenum * linenum -> token
    val OR : linenum * linenum -> token
    val ASSIGN : linenum * linenum -> token
    val SEMICOLON : linenum * linenum -> token
    val COMMA : linenum * linenum -> token
    val LPAREN : linenum * linenum -> token
    val RPAREN : linenum * linenum -> token
    val BAR : linenum * linenum -> token
    val ARROW : linenum * linenum -> token
    val IF : linenum * linenum -> token
    val END : linenum * linenum -> token
    val SKIP : linenum * linenum -> token
    val ABORT : linenum * linenum -> token
    val INT : linenum * linenum * int -> token
    val BOOL : linenum * linenum * bool -> token
    val IDENT : linenum * linenum * string -> token
    val EOF : linenum * linenum -> token
end (* signature TOKENS *)