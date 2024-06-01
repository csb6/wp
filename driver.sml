structure Driver = struct

fun printError filename (msg, startLine, endLine) = let
    val lineNums = if startLine = endLine then Int.toString startLine
                   else (Int.toString startLine) ^ ".1-" ^ (Int.toString endLine) ^ ".1"
in
    print (filename ^ ":" ^ lineNums ^ " Error: " ^ msg ^ "\n")
end

fun parse filename = let
    val file = TextIO.openIn filename
    fun printFileError linenum msg = printError filename (msg, linenum, linenum)
    fun get _ = TextIO.input file
    val lexer = GCLParser.makeLexer get
    fun run() = let
        val (result, _) = GCLParser.parse (0, lexer, printError filename, ()) in
        Check.typeCheckStmt result;
        TextIO.closeIn file;
        result
    end
in
    SOME (run()) handle
        GCLParser.LexerError (err, pos) => (case err of
            GCLParser.UnknownCharacter ch =>
                (printFileError pos ("Unexpected character: '" ^ (str ch) ^ "'"); NONE)
        )
      | GCLParser.ParseError =>
            (print "Parsing failed\n"; NONE)
      | Check.TypeError (errMsg, pos)  =>
            (printFileError pos errMsg; NONE)
end

end (* structure Driver *)