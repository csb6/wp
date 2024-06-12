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
structure Check = struct
    open Position_AST

    exception TypeError of string * pos

    fun unaryOpType oper = (case oper of
        Not => BoolType
    )

    (* Type of a binary expression with this operator *)
    fun binOpType oper = (case oper of
        Plus  => IntType
      | Minus => IntType
      | Mult  => IntType
      | Div   => IntType
      | Eq    => BoolType
      | Ne    => BoolType
      | And   => BoolType
      | Or    => BoolType
    )

    fun typeCheckOperand oper rightType = (case (oper, rightType) of
        (Not, BoolType) => true
      | _               => false
    )

    fun typeCheckOperands oper leftType rightType = (case (leftType, oper, rightType) of
        (IntType,  Plus,  IntType)   => true
      | (IntType,  Minus, IntType)   => true
      | (IntType,  Mult,  IntType)   => true
      | (IntType,  Div,   IntType)   => true
      | (IntType,  Eq,    IntType)   => true
      | (BoolType, Eq,    BoolType)  => true
      | (IntType,  Ne,    IntType)   => true
      | (BoolType, Ne,    BoolType)  => true
      | (BoolType, And,   BoolType)  => true
      | (BoolType, Or,    BoolType)  => true
      | _                            => false
    )

    fun typeCheckExpr expr = (case expr of
        Bool _                    => BoolType
      | Int _                     => IntType
      | VarExpr _                 => IntType
      | UnaryExpr (oper, r)       =>
            if not (typeCheckOperand oper (typeCheckExpr r)) then
                raise TypeError ("Operator in unary expression used with wrong type of operand", getExprData r)
            else
                unaryOpType oper
      | BinExpr (l, oper, r, pos) => let
            val leftType = typeCheckExpr l
            val rightType = typeCheckExpr r
        in
            if leftType <> rightType then
                raise TypeError ("Type mismatch in binary expression", pos)
            else if not (typeCheckOperands oper leftType rightType) then
                raise TypeError ("Operator in binary expression used with wrong type of operands", pos)
            else
                binOpType oper
        end
    )

    fun typeCheckStmt stmt = (case stmt of
        Skip _                     => ()
      | Abort _                    => ()
      | ExprStmt expr              => (typeCheckExpr expr; ())
      | Seq (s0, sx)               => List.app typeCheckStmt (s0::sx)
      | Assignment (assignList, _) => List.app typeCheckAssignment assignList
      | IfStmt ((gc0, gcx), _)     => List.app typeCheckGuardedCommand (gc0::gcx)
      | LoopStmt ((gc0, gcx), _)   => List.app typeCheckGuardedCommand (gc0::gcx)
    )
    and typeCheckAssignment (_, expr) =
        if typeCheckExpr expr <> IntType then
            raise TypeError ("Attempted to assign boolean expression to integer variable", getExprData expr)
        else
            ()
    and typeCheckGuardedCommand (guard, cmd) =
        if typeCheckExpr guard <> BoolType then
            raise TypeError ("Guard expressions must have type boolean", getExprData guard)
        else
            typeCheckStmt cmd

    (* Assumes expr has already been typechecked successfully *)
    fun typeOfExpr expr = (case expr of
        Bool _                  => BoolType
      | Int _                   => IntType
      | VarExpr _               => IntType
      | UnaryExpr (oper, _)     => unaryOpType oper
      | BinExpr (_, oper, _, _) => binOpType oper
    )
end (* structure Check *)