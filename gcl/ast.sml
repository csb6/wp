structure AST = struct

type pos = int
type symbol = Atom.atom

datatype type_ = BoolType | IntType

datatype variable = Var of symbol
fun makeVar name = Var (Atom.atom name)
fun sameVar (Var a) (Var b) = Atom.same (a, b)
fun getVarName (Var a) = Atom.toString a

datatype operator = Plus | Minus | Mult | Div | Eq | Ne | And | Or

datatype expression = Bool of bool * pos
                    | Int of int * pos
                    | VarExpr of variable * pos
                    | BinExpr of expression * operator * expression * pos

datatype statement = Skip of pos
                   | Abort of pos
                   | ExprStmt of expression
                   | Seq of statement list
                   | Assignment of variable * expression * pos
                   | IfStmt of guarded_command list * pos
withtype guarded_command = expression * statement

 (* TODO: consider using expression's pos as its canonical position and extending
 Assignment to hold a variable * expression * pos list. That way the line numbers align
 with the right-hand side expression *)

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
  | Seq (s1::_)            => getStmtPos s1
  | Seq []                 => raise Domain (* Parsing should guarantee [] case does not occur *)
  | Assignment (_, _, pos) => pos
  | IfStmt (_, pos)        => pos
)

end (* structure AST *)