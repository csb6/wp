structure WP = struct
open AST

exception todo

fun substituteExpr var newExpr expr = let
    val substExpr = substituteExpr var newExpr
in
    case expr of
      Bool b                    => Bool b
    | Int i                     => Int i
    | VarExpr (v, pos)          => if AST.same_var v var then newExpr else VarExpr (v, pos)
    | BinExpr (l, oper, r, pos) => BinExpr (substExpr l, oper, substExpr r, pos)
end

fun substituteStmt var newExpr stmt = let
    val substStmt = substituteStmt var newExpr
    val substExpr = substituteExpr var newExpr
in
    case stmt of
      Skip s                    => Skip s
    | Abort a                   => Abort a
    | ExprStmt expr             => ExprStmt (substExpr expr)
    | Seq (s1, s2)              => Seq (substStmt s1, substStmt s2)
    | Assignment (v, expr, pos) => Assignment (v, substExpr expr, pos)
end

fun wp stmt postCond = (case (stmt, postCond) of
    (_,                       Bool (false, pos)) => Bool (false, pos)
  | (Abort pos,               _)                 => Bool (false, pos)
  | (Skip _,                  r)                 => r
  | (Assignment (v, expr, _), r)                 => substituteExpr v expr r
  | _                                            => raise todo
)

end (* structure WP *)