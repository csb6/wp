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
            Not => "not "
        )

        fun binOpToString oper = (case oper of
            Plus  => " + "
          | Minus => " - "
          | Mult  => " * "
          | Div   => " / "
          | Eq    => " = "
          | Ne    => " != "
          | And   => " and "
          | Or    => " or "
        )
    end (* structure Basic_Types *)
    open Basic_Types

    datatype expression = Bool of bool
                        | Int of int
                        | VarExpr of variable
                        | UnaryExpr of unary_operator * expression
                        | BinExpr of expression * binary_operator * expression
                        (* WP of sequence of k IfStmts with a final postcondition, where k >= 0 *)
                        | WPIndefSeq of guarded_command list * expression
    and       statement = Skip
                        | Abort
                        | ExprStmt of expression
                        | Seq of statement List_Utils.nonempty_list
                        | Assignment of (variable * expression) list
                        | IfStmt of guarded_command List_Utils.nonempty_list
                        | LoopStmt of guarded_command List_Utils.nonempty_list
    withtype guarded_command = expression * statement

    fun exprToString expr = (case expr of
        Bool b                 => if b then "true" else "false"
      | Int i                  => Int.toString i
      | VarExpr v              => getVarName v
      | UnaryExpr (oper, e)    => (unaryOpToString oper) ^ (exprToString e)
      | BinExpr (l, oper, r)   => "(" ^ (exprToString l) ^ (binOpToString oper) ^ (exprToString r) ^ ")"
      | WPIndefSeq (gcList, r) => "wp(repeat " ^ (concat (map gcToString gcList)) ^ " until (" ^ (exprToString r) ^ "))"
    )
    and stmtToString stmt = (case stmt of
        Skip                 => "skip"
      | Abort                => "abort"
      | ExprStmt expr        => exprToString expr
      | Seq (s0, sx)         => String.concatWith ";\n" (map stmtToString (s0::sx))
      | Assignment assgnList =>
            (String.concatWith ", " (map (fn (v, _) => getVarName v) assgnList)) ^ " := "
          ^ (String.concatWith ", " (map (fn (_, e) => exprToString e) assgnList))
      | IfStmt (gc0, gcx)    => "if\n" ^ (concat (map gcToString (gc0::gcx))) ^ "\nend\n"
      | LoopStmt (gc0, gcx)  => "loop\n" ^ (concat (map gcToString (gc0::gcx))) ^ "\nend\n"
    )
    and gcToString (guard, stmt) =
        (exprToString guard) ^ " -> " ^ (stmtToString stmt) ^ "\n"
end (* structure AST *)

local
    structure AST_Matches_Interface : AST_INTERFACE = AST
in end