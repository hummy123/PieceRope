namespace Buffer

open Types
open Node
open Data

(* Contains AA Tree rebalancing logic, specific for buffer metadata. *)
module private AaTree =
    let inline skew node =
        match node with
        | BT(lvx, BT(lvy, a, lky, ky, rky, b), lkx, kx, rkx, c) as t when lvx = lvy ->
            let innerNode =  BT(lvx, b, rky, kx, rkx, c)
            BT(lvx, a, lky, ky, size innerNode, innerNode)
        | t -> t

    let inline split node =
        match node with
        | BT(lvx, a, lkx, kx, rkx, BT(lvy, b, lky, ky, rky, BT(lvz, c, lkz, kz, rkz, d))) as t when lvx = lvy && lvy = lvz -> 
            let right = BT(lvx, c, lkz, kz, rkz, d)
            let left = BT(lvx, a, lkx, kx, lky, b)
            BT(lvx + 1, left, size left, ky, size right, right)
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
            | BE -> BT(1, BE, 0, string, 0, BE) |> cont
            | BT(h, l, lm, v, rm, r) -> 
                insMax r (fun r' ->
                    BT(h, l, lm, v, rm + string.Length, r')
                    |> skew |> split |> cont
                )
        insMax tree topLevelCont

    /// Returns a substring from the rope at the given start index and length.
    let substring (start: int) (length: int) tree =
        let finish = start + length
        
        (* To do: maybe convert to CPS. *)
        let rec sub curIndex node (acc: string) =
            match node with
            | BE -> acc
            | BT(h, l, lm, v, rm, r) ->
                let left = 
                    if start < curIndex
                    then sub (curIndex - stringLength l - sizeRight l) l acc
                    else acc
                
                let nextIndex = curIndex + v.Length
                let middle =
                    if start <= curIndex && finish >= nextIndex then 
                        (* Node is fully in range. *)
                        left + v
                    elif start >= curIndex && finish <= nextIndex then
                        (* Range is within node. *)
                        let strStart = start - curIndex
                        left + v.Substring(strStart, length)
                    elif finish < nextIndex && finish >= curIndex then
                        (* Start of node is within range. *)
                        let length = finish - curIndex
                        left + v.Substring(0, length)
                    elif start > curIndex && start <= nextIndex then
                        (* End of node is within range. *)
                        let strStart = start - curIndex
                        left + v[strStart..]
                    else
                        left

                if finish > nextIndex
                then sub (nextIndex + sizeLeft r) r middle
                else middle

        sub (sizeLeft tree) tree ""
