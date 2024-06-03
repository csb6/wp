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
structure DecoratedAST = struct
    datatype 'a expression = Bool of bool * 'a
                           | Int of int * 'a
                           | VarExpr of AST.variable * 'a
                           | UnaryExpr of AST.unary_operator * 'a expression
                           | BinExpr of 'a expression * AST.binary_operator * 'a expression * 'a

    datatype 'a statement = Skip of 'a
                          | Abort of 'a
                          | ExprStmt of 'a expression
                          | Seq of 'a statement list
                          | Assignment of AST.variable * 'a expression * 'a
                          | IfStmt of 'a guarded_command list * 'a
    withtype 'a guarded_command = 'a expression * 'a statement

    (* Convert to AST.expression *)
    fun stripExpr expr = (case expr of
        Bool (b, _)             => AST.Bool b
      | Int (i, _)              => AST.Int i
      | VarExpr (v, _)          => AST.VarExpr v
      | UnaryExpr (oper, r)     => AST.UnaryExpr (oper, stripExpr r)
      | BinExpr (l, oper, r, _) => AST.BinExpr (stripExpr l, oper, stripExpr r)
    )

    (* Convert to AST.statement *)
    fun stripStmt stmt = (case stmt of
        Skip _                  => AST.Skip
      | Abort _                 => AST.Abort
      | ExprStmt expr           => AST.ExprStmt (stripExpr expr)
      | Seq s                   => AST.Seq (map stripStmt s)
      | Assignment (v, expr, _) => AST.Assignment (v, stripExpr expr)
      | IfStmt (gcList, _)      => AST.IfStmt (map (fn (guard, cmd) => (stripExpr guard, stripStmt cmd)) gcList)
    )

    fun getExprData expr = (case expr of
        Bool (_, data)          => data
      | Int (_, data)           => data
      | VarExpr (_, data)       => data
      | UnaryExpr (_, e)        => getExprData e
      | BinExpr (_, _, _, data) => data
    )

    fun getStmtData stmt = (case stmt of
        Skip data               => data
      | Abort data              => data
      | ExprStmt expr           => getExprData expr
      | Seq (s1::_)             => getStmtData s1
      | Seq []                  => raise Domain (* Parsing should guarantee [] case does not occur *)
      | Assignment (_, _, data) => data
      | IfStmt (_, data)        => data
    )

    fun exprToString expr = AST.exprToString (stripExpr expr)

    fun stmtToString stmt = AST.stmtToString (stripStmt stmt)

end (* structure DecoratedAST *)