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

    fun notExpr a = UnaryExpr (Not, a)

    val || = fn (a, b) => BinExpr (a, Or, b)
    infix ||

    val && = fn (a, b) => BinExpr (a, And, b)
    infix &&

    val --> = fn (a, b) => (notExpr a) || b
    infix -->

    fun firstEl (a, _) = a

    fun substituteExpr var newExpr expr = let
        val substExpr = substituteExpr var newExpr
        val substGC = substituteGC var newExpr
    in
        case expr of
            Bool b                 => Bool b
          | Int i                  => Int i
          | VarExpr v              => if AST.sameVar v var then newExpr else VarExpr v
          | UnaryExpr (oper, r)    => UnaryExpr (oper, substExpr r)
          | BinExpr (l, oper, r)   => BinExpr (substExpr l, oper, substExpr r)
          | WPIndefSeq (gcList, r) => WPIndefSeq (map substGC gcList, substExpr r)
    end
    and substituteStmt var newExpr stmt = let
        val substStmt = substituteStmt var newExpr
        val substExpr = substituteExpr var newExpr
        val substGC = substituteGC var newExpr
    in
        case stmt of
            Skip                 => Skip
          | Abort                => Abort
          | ExprStmt expr        => ExprStmt (substExpr expr)
          | Seq (s0, sx)         => Seq (substStmt s0, map substStmt sx)
          | Assignment assgnList => Assignment (map (fn (v, e) => (v, substExpr e)) assgnList)
          | IfStmt (gc0, gcx)    => IfStmt   (substGC gc0, map substGC gcx)
          | LoopStmt (gc0, gcx)  => LoopStmt (substGC gc0, map substGC gcx)
    end
    and substituteGC var newExpr (guard, cmd) =
        (substituteExpr var newExpr guard, substituteStmt var newExpr cmd)

    fun wp stmt postCond = let
        fun wpSeqStmt s r = let
            val (s0, sx) = List_Utils.revNonEmpty s
        in
            foldl (fn (s', r') => wp s' r') (wp s0 r) sx
        end

        fun wpIfStmt ((g0, c0), gcx) r =
            foldl (op ||)  g0              (map  firstEl                    gcx) (* At least one guard g_x is true *)
         && foldl (op &&) (g0 --> wp c0 r) (map (fn (g, c) => g --> wp c r) gcx) (* g_x -> wp(c_x, r) *)

        fun wpLoopStmt ((g0, c0), gcx) r =
            WPIndefSeq (
                (g0, c0)::gcx,                                     (* k repetitions of if-statement *)
                 r && notExpr (foldl (op ||) g0 (map firstEl gcx)) (* final postcondition (when k = 0) *)
            )
    in
        case (stmt, postCond) of
            (_,                    Bool false) => Bool false
          | (Abort,                _)          => Bool false
          | (Skip,                 r)          => r
          | (ExprStmt _,           r)          => r
          | (Assignment assgnList, r)          => foldl (fn ((v, e), r') => substituteExpr v e r') r assgnList
          | (Seq s,                r)          => wpSeqStmt s r
          | (IfStmt gcList,        r)          => wpIfStmt gcList r
          | (LoopStmt gcList,      r)          => wpLoopStmt gcList r
    end
end (* structure WP *)