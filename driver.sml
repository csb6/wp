(*
WP: guarded command language tools
Copyright (C) 2024  Cole Blakley

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)
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
            GCLParser.LexerError (errMsg, pos) => (printFileError pos errMsg; NONE)
          | GCLParser.ParseError               => (print "Parsing failed\n"; NONE)
          | Check.TypeError (errMsg, pos)      => (printFileError pos errMsg; NONE)
    end
end (* structure Driver *)