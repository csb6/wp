structure AST = struct

type pos = int
type symbol = Atom.atom

datatype type_ = BoolType | IntType

datatype variable = Var of symbol
fun same_var (Var a) (Var b) = Atom.same (a, b)

datatype operator = Plus | Minus | Mult | Div | Eq | Ne | And | Or

datatype expression = Bool of bool * pos
                    | Int of int * pos
                    | VarExpr of variable * pos
                    | BinExpr of expression * operator * expression * pos

datatype statement = Skip of pos
                   | Abort of pos
                   | ExprStmt of expression
                   | Seq of statement * statement
                   | Assignment of variable * expression * pos
                   | IfStmt of guarded_command list * pos
withtype guarded_command = expression * statement

fun getExprPos expr = (case expr of
    Bool (_, pos)          => pos
  | Int (_, pos)           => pos
  | VarExpr (_, pos)       => pos
  | BinExpr (_, _, _, pos) => pos
)

fun getStmtPos stmt = (case stmt of
    Skip pos               => pos
  | Abort pos              => pos
  | ExprStmt expr          => getExprPos expr
  | Seq (s1, _)            => getStmtPos s1
  | Assignment (_, _, pos) => pos
  | IfStmt (_, pos)        => pos
)

end (* structure AST *)