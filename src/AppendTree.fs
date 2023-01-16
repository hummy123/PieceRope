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

    /// Returns a substring from the rope at the given start index and length.
    let substring (start: int) (length: int) rope =
        let finish = start + length
        let acc = ResizeArray<char>() (* Using mutable array for performance. *)
        
        (* To do: maybe convert to CPS. *)
        let rec sub curIndex node =
            match node with
            | E -> ()
            | T(h, l, v, r) ->
                if start < curIndex
                then sub (curIndex - stringLength l - sizeRight l) l
                let nextIndex = curIndex + v.String.Length

                if start <= curIndex && finish >= nextIndex then 
                    (* Node is fully in range. *)
                    for i in v.String do
                        acc.Add i
                elif start >= curIndex && finish <= nextIndex then
                    (* Range is within node. *)
                    let strStart = start - curIndex
                    let length = finish - start
                    for i in v.String.Substring(strStart, length) do
                        acc.Add i
                elif finish < nextIndex && finish >= curIndex then
                    (* Start of node is within range. *)
                    let length = finish - curIndex
                    for i in v.String.Substring(0, length) do
                        acc.Add i
                elif start > curIndex && start <= nextIndex then
                    (* End of node is within range. *)
                    let strStart = start - curIndex
                    for i in v.String[strStart..] do
                        acc.Add i

                if finish > nextIndex
                then sub (nextIndex + sizeLeft r) r

        sub (sizeLeft rope) rope
        new string(acc.ToArray())