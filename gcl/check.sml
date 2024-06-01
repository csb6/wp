structure Check = struct
open AST

exception TypeError of string * pos

(* Type of a binary expression with this operator *)
fun operatorType oper = (case oper of
      Plus  => IntType
    | Minus => IntType
    | Mult  => IntType
    | Div   => IntType
    | Eq    => BoolType
    | Ne    => BoolType
    | And   => BoolType
    | Or    => BoolType
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
    Bool _               => BoolType
  | Int _                => IntType
  | VarExpr _            => IntType
  | BinExpr (l, oper, r, pos) => let
        val leftType = typeCheckExpr l
        val rightType = typeCheckExpr r
    in
        if leftType <> rightType then
            raise TypeError ("Type mismatch in binary expression", pos)
        else if not (typeCheckOperands oper leftType rightType) then
            raise TypeError ("Operator in binary expression used with wrong type of operands", pos)
        else
            operatorType oper
    end
)

fun typeCheckStmt stmt = (case stmt of
    Skip _               => ()
  | Abort _              => ()
  | ExprStmt expr        => (typeCheckExpr expr; ())
  | Seq (first, next)    => (typeCheckStmt first; typeCheckStmt next; ())
  | Assignment (_, expr, pos) =>
    if typeCheckExpr expr <> IntType then
        raise TypeError ("Attempted to assign boolean expression to integer variable", pos)
    else
        ()
  | IfStmt (gcList, _) => List.app typeCheckGuardedCommand gcList
)
and typeCheckGuardedCommand (guard, cmd) =
    if typeCheckExpr guard <> BoolType then
      raise TypeError ("Guard expressions must have type boolean", getExprPos guard)
    else
      typeCheckStmt cmd

(* Assumes expr has already been typechecked successfully *)
fun typeOfExpr expr = (case expr of
    Bool _                  => BoolType
  | Int _                   => IntType
  | VarExpr _               => IntType
  | BinExpr (_, oper, _, _) => operatorType oper
)

end (* structure Check *)