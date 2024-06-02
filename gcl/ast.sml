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

fun opToString oper = (case oper of
    Plus  => "+"
  | Minus => "-"
  | Mult  => "*"
  | Div   => "/"
  | Eq    => "="
  | Ne    => "!="
  | And   => "and"
  | Or    => "or"
)

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
    Bool (b, _)             => if b then "true" else "false"
  | Int (i, _)              => Int.toString i
  | VarExpr (v, _)          => getVarName v
  | BinExpr (l, oper, r, _) => "(" ^ (exprToString l) ^ (opToString oper) ^ (exprToString r) ^ ")"
)

fun stmtToString stmt = (case stmt of
    Skip _                  => "skip"
  | Abort _                 => "abort"
  | ExprStmt expr           => exprToString expr
  | Seq s                   => (joinStr ";\n" (map stmtToString s)) ^ "\n"
  | Assignment (v, expr, _) => (getVarName v) ^ " := " ^ (exprToString expr)
  | IfStmt (gcList, _)      => "if\n" ^ (concat (map gcToString gcList)) ^ "end\n"
)
and gcToString (guard, stmt) =
  (exprToString guard) ^ " -> " ^ (stmtToString stmt) ^ "\n"

end (* structure AST *)