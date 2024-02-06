module type KVType = sig
  type k
  type v
end

module type SplayListType = sig
  type k
  type v
  type node
    = N of k * v ref * node ref * node ref * node ref * node ref array
    | Ept
  type sl = SL of (node ref)
  val nodeConstructor : k -> v -> node ref -> node ref -> node ref -> node
  val idxAtParent : node -> int
  val maxNode : node -> node
  val minNode : node -> node
  val maintainParent : node -> int -> unit
  val swapChildren : node -> int -> node ref -> int -> unit
  val remove : node -> sl -> unit
  val rotate : node -> node ref -> unit
  val splay : node -> node ref -> unit
  (* val rootNode : node ref *)
  val findNodeWithoutSplay : sl -> k -> node
  val findNode : sl -> k -> node
  val findLeNode : sl -> k -> node
  val findPreciseNode : sl -> k -> node
  val findLe : sl -> k -> v option
  val findPrecise : sl -> k -> v option
  val hasLe : sl -> k -> bool
  val hasPrecise : sl -> k -> bool
  val removeLe : sl -> k -> unit
  val removePrecise : sl -> k -> unit
  val insert : sl -> k -> v -> unit
end

module SplayList(KV : KVType) : SplayListType = struct
  type k = KV.k
  type v = KV.v
  type node
    = N of k * v ref * node ref * node ref * node ref * node ref array
    | Ept
  type sl = SL of node ref
  let nodeConstructor theK theV parent children splayParent = (
    let
      this = N(theK, ref theV, parent, children, splayParent, [|ref Ept; ref Ept|])
    in (
      if !parent <> Ept then match !parent with N(_, _, p, _, _, _) ->
        match !p with N(_, _, _, c, _, _) -> c := this
      else ();
      if !children <> Ept then match !children with N(_, _, _, c, _, _) ->
        match !c with N(_, _, p, _, _, _) -> p := this
      else ();
      this
    )
  )
  let idxAtParent (N(_, _, _, _, splayParent, _) as this) =
    match !splayParent with N(_, _, _, _, _, splayChildren) ->
      if !(splayChildren.(0)) = this then 0 else 1
  let maxNode (N(_, _, _, _, _, splayChildren) as this) = (
    let ptr = ref this
    in (
      while (match !ptr with N(_, _, _, _, _, sc) -> !(sc.(1))) <> Ept do
        ptr := match !ptr with N(_, _, _, _, _, sc) -> !(sc.(1))
      done;
      !ptr
    )
  )
  let minNode (N(_, _, _, _, _, splayChildren) as this) = (
    let ptr = ref this
    in (
      while (match !ptr with N(_, _, _, _, _, sc) -> !(sc.(0))) <> Ept do
        ptr := match !ptr with N(_, _, _, _, _, sc) -> !(sc.(0))
      done;
      !ptr
    )
  )
  let maintainParent (N(_, _, _, _, _, splayChildren) as this) idx =
    if !(splayChildren.(idx)) <> Ept then
      match !(splayChildren.(idx)) with N(_, _, _, _, sp, _) ->
        sp :=this
    else ()
  let swapChildren (N(_, _, _, _, _, splayChildren) as this) idx nd nidx = (
    let mid = !(splayChildren.(idx))
    in (
      splayChildren.(idx) := (match !nd with N(_, _, _, _, _, nsc) -> !(nsc.(nidx)));
      match !nd with N(_, _, _, _, _, nsc) -> nsc.(nidx) := mid;
      maintainParent this idx;
      maintainParent !nd nidx
    )
  )
  let rotate (N(_, _, _, _, splayParent, _) as this) rtndref = (
    let splayParentBackup = splayParent in
    let idx = idxAtParent this
    in (
      if (match !splayParent with N(_, _, _, _, sp, _) -> !sp) <> Ept then
        let spt = (match !splayParent with N(_, _, _, _, sp, _) -> sp)
        in swapChildren !spt (idxAtParent !splayParent) splayParent idx
      else (
        let mid = !rtndref
        in (
          rtndref := (match !splayParent with N(_, _, _, _, _, sc) -> !(sc.(idx)));
          match !splayParent with N(_, _, _, _, _, sc) -> sc.(idx) := mid
        );
        splayParent := Ept
      );
      swapChildren !splayParentBackup idx (ref this) (idx lxor 1)
    )
  )
  let splay (N(_, _, _, _, splayParent, _) as this) rtndref =
    while !splayParent <> Ept do
      if (match !splayParent with N(_, _, _, _, sp, _) -> !sp) <> Ept then
        if idxAtParent this = idxAtParent !splayParent then
          rotate !splayParent rtndref
        else
          rotate this rtndref
      else ()
    done
  let remove (N(_, _, parent, children, splayParent, splayChildren) as this)
    (SL(rtndref) as tl) = (
    if !parent <> Ept then
      match !parent with N(_, _, _, c, _, _) -> c := !children
    else ();
    if !children <> Ept then
      match !children with N(_, _, p, _, _, _) -> p := !parent
    else ();
    splay this rtndref;
    if !(splayChildren.(0)) = Ept && !(splayChildren.(1)) = Ept then
      rtndref := Ept
    else if !(splayChildren.(0)) = Ept then (
      rtndref := !(splayChildren.(1));
      match !rtndref with N(_, _, _, _, sp, _) -> sp := Ept
    ) else if !(splayChildren.(1)) = Ept then (
      rtndref := !(splayChildren.(0));
      match !rtndref with N(_, _, _, _, sp, _) -> sp := Ept
    ) else (
      match !(splayChildren.(1)) with N(_, _, _, _, sp, _) -> sp := Ept;
      let mn = minNode !(splayChildren.(1))
      in splay mn splayChildren.(1);
      match !(splayChildren.(1)) with N(_, _, _, _, _, sc) ->
        splayChildren.(0) := !(splayChildren.(0));
      maintainParent !(splayChildren.(1)) 0;
      rtndref := !(splayChildren.(1))
    )
  )
  let findNodeWithoutSplay (SL(rtndref) as this) theK =
    if !rtndref = Ept then
      Ept
    else
      let lastPtr = ref Ept in
      let ptr = rtndref in
      let ret = ref Ept in
      let flag = ref 1
      in (
        while !ptr <> Ept do
          if (match !ptr with N(rk, _, _, _, _, _) -> rk) = theK then (
            ret := !ptr;
            flag := 2
          ) else
            let idx = if theK < (match !ptr with N(rk, _, _, _, _, _) -> rk) then 0 else 1
            in (
              lastPtr := !ptr;
              ptr := (match !ptr with N(_, _, _, _, _, sc) -> !(sc.(idx)))
            )
        done;
        if !flag = 2 then !ret else !lastPtr
      )
  let findNode (SL(rtndref) as this) theK =
    let ret = findNodeWithoutSplay this theK
    in (
      if ret <> Ept then
        splay ret rtndref
      else ();
      ret
    )
  let findLeNode (SL(rtndref) as this) theK =
    let ptr = findNode this theK
    in
      if ptr = Ept then
        Ept
      else if (match ptr with N(pk, _, _, _, _, _) -> pk) <= theK then
        ptr
      else
        if (match ptr with N(_, _, _, _, _, sc) -> !(sc.(0))) = Ept then
          Ept
        else
          let ret = maxNode (match ptr with N(_, _, _, _, _, sc) -> !(sc.(0)))
          in (
            splay ret rtndref;
            ret
          )
  let findPreciseNode (SL(rtndref) as this) theK =
    let ptr = findNode this theK in
      if ptr = Ept || (match ptr with N(pk, _, _, _, _, _) -> pk) <> theK then Ept else ptr
  let findLe (SL(rtndref) as this) theK =
    let ptr = findLeNode this theK in
      if ptr = Ept then None else (match ptr with N(_, pv, _, _, _, _) -> Some !pv)
  let findPrecise (SL(rtndref) as this) theK =
    let ptr = findPreciseNode this theK in
      if ptr = Ept then None else (match ptr with N(_, pv, _, _, _, _) -> Some !pv)
  let hasLe (SL(rtndref) as this) theK =
    let ptr = findLeNode this theK in ptr <> Ept
  let hasPrecise (SL(rtndref) as this) theK =
    let ptr = findNode this theK in
      ptr <> Ept && (match ptr with N(pk, _, _, _, _, _) -> pk) = theK
  let removeLe (SL(rtndref) as this) theK =
    let ptr = findLeNode this theK in
      if ptr <> Ept then
        remove ptr this
      else ()
  let removePrecise (SL(rtndref) as this) theK =
    let ptr = findPreciseNode this theK in
      if ptr <> Ept then
        remove ptr this
      else ()
  let insert (SL(rtndref) as this) theK theV =
    let ptr = findNodeWithoutSplay this theK in
      if ptr <> Ept then
        if (match ptr with N(pk, _, _, _, _, _) -> pk) < theK then
          match ptr with N(_, _, _, _, _, sc) -> sc.(1) :=
            nodeConstructor theK theV (ref ptr) (match ptr with N(_, _, _, c, _, _) -> c) (ref ptr)
        else if theK < (match ptr with N(pk, _, _, _, _, _) -> pk) then
          match ptr with N(_, _, _, _, _, sc) -> sc.(0) :=
            nodeConstructor theK theV (match ptr with N(_, _, p, _, _, _) -> p) (ref ptr) (ref ptr)
        else
          match ptr with N(_, pv, _, _, _, _) -> pv := theV
      else
        rtndref := nodeConstructor theK theV (ref Ept) (ref Ept) (ref Ept)
end
