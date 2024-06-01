structure WP = struct

type operator = AST.operator
datatype expression = Bool of bool
                    | Int of int
                    | VarExpr of AST.variable
                    | BinExpr of expression * operator * expression

datatype statement = Skip
                   | Abort
                   | ExprStmt of expression
                   | Seq of statement list
                   | Assignment of AST.variable * expression
                   | IfStmt of guarded_command list
withtype guarded_command = expression * statement

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
  | AST.Seq s                   => Seq (map gclToWpStmt s)
  | AST.Assignment (v, expr, _) => Assignment (v, gclToWpExpr expr)
  | AST.IfStmt (gcList, _)      => IfStmt (map (fn (guard, cmd) => (gclToWpExpr guard, gclToWpStmt cmd)) gcList)
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
    | Seq s                => Seq (map substStmt s)
    | Assignment (v, expr) => Assignment (v, substExpr expr)
    | IfStmt _             => raise todo
end

fun wp stmt postCond = (case (stmt, postCond) of
    (_,                    Bool false) => Bool false
  | (Abort,                _)          => Bool false
  | (Skip,                 r)          => r
  | (Assignment (v, expr), r)          => substituteExpr v expr r
  | (Seq s,                r)          => wpSeqStmt s r
  | _                                  => raise todo
)
and wpSeqStmt s r = (case rev s of
    s0::sx => foldl (fn (s', r') => wp s' r') (wp s0 r) sx
  | []     => raise Domain
)

end (* structure WP *)