structure List_Utils = struct

    type 'a nonempty_list = 'a * 'a list

    fun revNonEmpty (l0, lx) = (case rev (l0::lx) of
        l0'::lx' => (l0', lx')
      | []       => raise Domain
    )

end (* structure List_Utils *)