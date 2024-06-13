structure String_Utils = struct

    fun join sep lst = let
        fun join' lst' soFar = (case lst' of
            []         => soFar
          | [head]     => soFar ^ head
          | head::tail => join' tail (soFar ^ head ^ sep)
        )
    in
        join' lst ""
    end

end (* structure String_Utils *)