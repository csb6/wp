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
signature AST_INTERFACE = sig
    type symbol
    type type_
    type variable
    type binary_operator
    type unary_operator
    type expression
    type statement
    type guarded_command

    val makeVar : string -> variable
    val sameVar : variable -> variable -> bool
    val getVarName : variable -> string
    val unaryOpToString : unary_operator -> string
    val binOpToString : binary_operator -> string

    val exprToString : expression -> string
    val stmtToString : statement -> string
end (* signature AST_INTERFACE *)