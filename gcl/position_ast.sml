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
structure Position_AST = struct
    open AST.Basic_Types
    open DecoratedAST

    type pos = int
    type statement = pos DecoratedAST.statement
    type expression = pos DecoratedAST.expression
    type guarded_command = pos DecoratedAST.guarded_command

    exception CustomParseError of string * pos
end (* structure Position_AST *)

local
    structure Position_AST_Matches_Interface : AST_INTERFACE = Position_AST
in end