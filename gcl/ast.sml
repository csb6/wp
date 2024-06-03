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
structure AST = struct
    structure Basic_Types = struct
        type symbol = Atom.atom

        datatype type_ = BoolType | IntType
        datatype variable = Var of symbol
        datatype binary_operator = Plus | Minus | Mult | Div | Eq | Ne | And | Or
        datatype unary_operator = Not

        fun makeVar name = Var (Atom.atom name)
        fun sameVar (Var a) (Var b) = Atom.same (a, b)
        fun getVarName (Var a) = Atom.toString a

        fun unaryOpToString oper = (case oper of
            Not => "not"
        )

        fun binOpToString oper = (case oper of
            Plus  => "+"
          | Minus => "-"
          | Mult  => "*"
          | Div   => "/"
          | Eq    => "="
          | Ne    => "!="
          | And   => "and"
          | Or    => "or"
        )
    end (* structure Basic_Types *)
    open Basic_Types

    datatype expression = Bool of bool
                        | Int of int
                        | VarExpr of variable
                        | UnaryExpr of unary_operator * expression
                        | BinExpr of expression * binary_operator * expression

    datatype statement = Skip
                       | Abort
                       | ExprStmt of expression
                       | Seq of statement list
                       | Assignment of variable * expression
                       | IfStmt of guarded_command list
    withtype guarded_command = expression * statement

    (* TODO: consider using expression's pos as its canonical position and extending
    Assignment to hold a variable * expression * pos list. That way the line numbers align
    with the right-hand side expression *)

    fun joinStr sep lst = let
        fun join lst' soFar = (case lst' of
            []         => soFar
          | [head]     => soFar ^ head
          | head::tail => join tail (soFar ^ head ^ sep)
        )
    in
        join lst ""
    end

    fun exprToString expr = (case expr of
        Bool b               => if b then "true" else "false"
      | Int i                => Int.toString i
      | VarExpr v            => getVarName v
      | UnaryExpr (oper, e)  => (unaryOpToString oper) ^ " " ^ (exprToString e)
      | BinExpr (l, oper, r) => "(" ^ (exprToString l) ^ (binOpToString oper) ^ (exprToString r) ^ ")"
    )

    fun stmtToString stmt = (case stmt of
        Skip                 => "skip"
      | Abort                => "abort"
      | ExprStmt expr        => exprToString expr
      | Seq s                => (joinStr ";\n" (map stmtToString s)) ^ "\n"
      | Assignment (v, expr) => (getVarName v) ^ " := " ^ (exprToString expr)
      | IfStmt gcList        => "if\n" ^ (concat (map gcToString gcList)) ^ "end\n"
    )
    and gcToString (guard, stmt) =
        (exprToString guard) ^ " -> " ^ (stmtToString stmt) ^ "\n"
end (* structure AST *)

local
    structure AST_Matches_Interface : AST_INTERFACE = AST
in end