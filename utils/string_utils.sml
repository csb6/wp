structure String_Utils = struct

    fun join sep lst = (case lst of
        []         => ""
      | head::tail => foldl (fn (x, soFar) => soFar ^ sep ^ x) head tail
    )

end (* structure String_Utils *)