namespace AppendRope

open AppendRopeTypes
open RopeNode
open RopeData
open AaTree

module AppendTree =
    /// Used for CPS.
    let inline topLevelCont t = t

    /// Returns an empty AppendTree.
    let empty = E

    /// Append a string to an AppendTree.
    let append string node = 
        let rec insMax node cont =
            match node with
            | E -> T(1, E, RopeNode.create string, E) |> cont
            | T(h, l, v, r) -> 
                insMax r (fun r' ->
                    let v = v.AddRight string.Length
                    T(h, l, v, r')
                    |> skew |> split |> cont
                )
        insMax node topLevelCont