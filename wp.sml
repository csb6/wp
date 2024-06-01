structure WP = struct

type operator = AST.operator
datatype expression = Bool of bool
                    | Int of int
                    | VarExpr of AST.variable
                    | BinExpr of expression * operator * expression

datatype statement = Skip
                   | Abort
                   | ExprStmt of expression
                   | Seq of statement * statement
                   | Assignment of AST.variable * expression

exception todo

fun gclToWpExpr expr = (case expr of
    AST.Bool (b, _)             => Bool b
  | AST.Int (i, _)              => Int i
  | AST.VarExpr (v, _)          => VarExpr v
  | AST.BinExpr (l, oper, r, _) => BinExpr (gclToWpExpr l, oper, gclToWpExpr r)
)

fun gclToWpStmt stmt = (case stmt of
    AST.Skip _                  => Skip
  | AST.Abort _                 => Abort
  | AST.ExprStmt expr           => ExprStmt (gclToWpExpr expr)
  | AST.Seq (s1, s2)            => Seq (gclToWpStmt s1, gclToWpStmt s2)
  | AST.Assignment (v, expr, _) => Assignment (v, gclToWpExpr expr)
)

fun substituteExpr var newExpr expr = let
    val substExpr = substituteExpr var newExpr
in
    case expr of
      Bool b               => Bool b
    | Int i                => Int i
    | VarExpr v            => if AST.same_var v var then newExpr else VarExpr v
    | BinExpr (l, oper, r) => BinExpr (substExpr l, oper, substExpr r)
end

fun substituteStmt var newExpr stmt = let
    val substStmt = substituteStmt var newExpr
    val substExpr = substituteExpr var newExpr
in
    case stmt of
      Skip                 => Skip
    | Abort                => Abort
    | ExprStmt expr        => ExprStmt (substExpr expr)
    | Seq (s1, s2)         => Seq (substStmt s1, substStmt s2)
    | Assignment (v, expr) => Assignment (v, substExpr expr)
end

fun wp stmt postCond = (case (stmt, postCond) of
    (_,                    Bool false) => Bool false
  | (Abort,                _)          => Bool false
  | (Skip,                 r)          => r
  | (Assignment (v, expr), r)          => substituteExpr v expr r
  | _                                  => raise todo
)

end (* structure WP *)