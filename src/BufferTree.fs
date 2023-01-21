namespace Buffer

open Types
open Node
open Data

(* Contains AA Tree rebalancing logic, specific for buffer metadata. *)
module private AaTree =
    let inline skew node =
        match node with
        | BT(lvx, BT(lvy, a, ky, b), kx, c) as t when lvx = lvy ->
            let kx = kx.SetIdx (size b) (size c)
            let innerNode =  BT(lvx, b, kx, c)
            let ky = ky.SetIdx (size a) (size innerNode)
            BT(lvx, a, ky, innerNode)
        | t -> t

    let inline split node =
        match node with
        | BT(lvx, a, kx, BT(lvy, b, ky, BT(lvz, c, kz, d))) as t when lvx = lvy && lvy = lvz -> 
            let right = BT(lvx, c, kz, d)
            let kx = kx.SetIdx (size a) (size b)
            let left = BT(lvx, a, kx, b)
            let ky = ky.SetIdx (size left) (size right)
            BT(lvx + 1, left, ky, right)
        | t -> t

open AaTree

(* Contains core algorithms, append and substring, for buffer. *)
module Tree =
    /// Used for CPS.
    let inline topLevelCont t = t

    /// Returns an empty BufferTree.
    let empty = BE

    /// Append a string to an BufferTree.
    let append string tree = 
        let rec insMax node cont =
            match node with
            | BE -> BT(1, BE, Node.create string, BE) |> cont
            | BT(h, l, v, r) -> 
                insMax r (fun r' ->
                    let v = v.AddRight string.Length
                    BT(h, l, v, r')
                    |> skew |> split |> cont
                )
        insMax tree topLevelCont

    /// Returns a substring from the rope at the given start index and length.
    let substring (start: int) (length: int) tree =
        let finish = start + length
        let acc = ResizeArray<char>() (* Using mutable array for performance. *)
        
        (* To do: maybe convert to CPS. *)
        let rec sub curIndex node =
            match node with
            | BE -> ()
            | BT(h, l, v, r) ->
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

        sub (sizeLeft tree) tree
        new string(acc.ToArray())
