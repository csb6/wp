structure Check = struct
open AST

datatype type_error_type = TypeMismatch of expression * operator * expression
                         | OpTypeMismatch of expression * operator * expression
                         | AssignTypeMismatch of variable * expression

exception TypeError of type_error_type * pos

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
            raise TypeError (TypeMismatch (l, oper, r), pos)
        else if not (typeCheckOperands oper leftType rightType) then
            raise TypeError (OpTypeMismatch (l, oper, r), pos)
        else
            leftType
    end
)

fun typeCheckStmt stmt = (case stmt of
    Skip _               => ()
  | Abort _              => ()
  | ExprStmt expr        => (typeCheckExpr expr; ())
  | Seq (first, next)    => (typeCheckStmt first; typeCheckStmt next; ())
  | Assignment (v, expr, pos) =>
    if typeCheckExpr expr <> IntType then
        raise TypeError (AssignTypeMismatch (v, expr), pos)
    else
        ()
)

(* Assumes expr has already been typechecked successfully *)
fun typeOfExpr expr = (case expr of
    Bool _                  => BoolType
  | Int _                   => IntType
  | VarExpr _               => IntType
  | BinExpr (_, oper, _, _) => operatorType oper
)

end (* structure Check *)