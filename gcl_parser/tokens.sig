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
    val BLOCK : linenum * linenum -> token
    val SKIP : linenum * linenum -> token
    val ABORT : linenum * linenum -> token
    val INT : linenum * linenum * int -> token
    val BOOL : linenum * linenum * bool -> token
    val IDENT : linenum * linenum * string -> token
    val EOF : linenum * linenum -> token
end (* signature TOKENS *)