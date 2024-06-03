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
structure WP = struct
    open AST

    val == = fn(a, b) => BinExpr (a, Eq, b)
    infix ==

    val != = fn(a, b) => BinExpr (a, Ne, b)
    infix !=

    val || = fn (a, b) => BinExpr (a, Or, b)
    infix ||

    val && = fn (a, b) => BinExpr (a, And, b)
    infix &&

    val --> = fn (a, b) => (UnaryExpr (Not, a)) || b
    infix -->

    exception todo

    fun substituteExpr var newExpr expr = let
        val substExpr = substituteExpr var newExpr
    in
        case expr of
            Bool b               => Bool b
          | Int i                => Int i
          | VarExpr v            => if AST.sameVar v var then newExpr else VarExpr v
          | UnaryExpr (oper, r)  => UnaryExpr (oper, substExpr r)
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

    fun wp stmt postCond = let
        fun wpSeqStmt s r = (case rev s of
            s0::sx => foldl (fn (s', r') => wp s' r') (wp s0 r) sx
          | []     => raise Domain
        )
        fun wpIfStmt gcList r = (case gcList of
            (g0, c0)::gcx =>
                foldl (op ||)  g0              (map (fn (g, _) => g)            gcx) (* At least one guard g_x is true *)
             && foldl (op &&) (g0 --> wp c0 r) (map (fn (g, c) => g --> wp c r) gcx) (* g_x -> wp(c_x, r) *)
          | []            => raise Domain
        )
    in
        case (stmt, postCond) of
            (_,                    Bool false) => Bool false
          | (Abort,                _)          => Bool false
          | (Skip,                 r)          => r
          | (Assignment (v, expr), r)          => substituteExpr v expr r
          | (Seq s,                r)          => wpSeqStmt s r
          | (IfStmt gcList,        r)          => wpIfStmt gcList r
          | _                                  => raise todo
    end
end (* structure WP *)