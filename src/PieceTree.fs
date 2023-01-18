namespace PieceTree

open Types
open Node
open Data

(* Contains core AA Tree balancing logic. *)
module private AaTree =
    let inline sngl node =
        match node with
        | PE -> false
        | PT(_, _, _, PE) -> true
        | PT(lvx, _, _, PT(lvy, _, _, _)) -> lvx > lvy

    let inline lvl node =
        match node with
        | PE -> 0
        | PT(lvt, _, _, _) -> lvt

    let inline skew node =
        match node with
        | PT(lvx, PT(lvy, a, ky, b), kx, c) as t when lvx = lvy ->
            let kx = kx.SetIdx (size b) (size c)
            let innerNode =  PT(lvx, b, kx, c)
            let ky = ky.SetIdx (size a) (size innerNode)
            PT(lvx, a, ky, innerNode)
        | t -> t

    let inline split node =
        match node with
        | PT(lvx, a, kx, PT(lvy, b, ky, PT(lvz, c, kz, d))) as t when lvx = lvy && lvy = lvz -> 
            let right = PT(lvx, c, kz, d)
            let kx = kx.SetIdx (size a) (size b)
            let left = PT(lvx, a, kx, b)
            let ky = ky.SetIdx (size left) (size right)
            PT(lvx + 1, left, ky, right)
        | t -> t

    let inline nlvl node =
        match node with
        | PT(lvt, _, _, _) as t -> if sngl t then lvt else lvt + 1
        | _ -> failwith "unexpected nlvl case"

    let inline adjust node =
        match node with
        | PT(lvt, lt, kt, rt) as t when lvl lt >= lvt - 1 && lvl rt >= (lvt - 1) -> 
            t
        | PT(lvt, lt, kt, rt) when lvl rt < lvt - 1 && sngl lt -> 
            PT(lvt - 1, lt, kt, rt) |> skew
        | PT(lvt, PT(lv1, a, kl, PT(lvb, lb, kb, rb)), kt, rt) when lvl rt < lvt - 1 -> 
            let kl = kl.SetIdx (size a) (size lb)
            let leftNode = PT(lv1, a, kl, lb)
            let kt = kt.SetIdx (size rb) (size rt)
            let rightNode = PT(lvt - 1, rb, kt, rt)
            let kb = kb.SetIdx (size leftNode) (size rightNode)
            PT(lvb + 1, leftNode, kb, rightNode)
        | PT(lvt, lt, kt, rt) when lvl rt < lvt -> 
            PT(lvt - 1, lt, kt, rt) |> split
        | PT(lvt, lt, kt, PT(lvr, (PT(lva, c, ka, d) as a), kr, b)) ->
            let kt = kt.SetIdx (size lt) (size c)
            let leftNode = PT(lvt - 1, lt, kt, c)
            let kr = kr.SetIdx (size d) (size b)
            let rightNode = PT(nlvl a, d, kr, b) |> split
            let ka = ka.SetIdx (size leftNode) (size rightNode)
            PT(lva + 1, leftNode, ka, rightNode)
        | _ -> node

    let rec splitMax =
        function
        | PT(_, l, v, PE) -> 
            let v = 
                { v with 
                    LeftIdx = size l; 
                    RightIdx = 0; }
            l, v
        | PT(h, l, v, r) -> 
            match splitMax r with
            | r', b -> 
                let v' = { v with RightIdx = size r' }
                let tree = PT(h, l, v', r') |> adjust
                let b' = { b with RightIdx = size tree }
                tree, b'
        | _ -> failwith "unexpected splitMax case"

    let rec foldOpt (f: OptimizedClosures.FSharpFunc<_, _, _>) x t =
        match t with
        | PE -> x
        | PT(_, l, v, r) ->
            let x = foldOpt f x l
            let x = f.Invoke(x, v)
            foldOpt f x r

    let fold f x t =
        foldOpt (OptimizedClosures.FSharpFunc<_, _, _>.Adapt (f)) x t

open AaTree

(* Contains core PieceTree logic. *)
module PieceTree =
    let empty = PE

    let text piecerope =
        fold (fun acc pc ->
            let text = Buffer.Tree.substring pc.Start pc.Length piecerope.Buffer
            acc + text) "" piecerope.Tree

    /// Used for CPS.
    let inline topLevelCont t = t

    let private insMin pcStart pcLength tree =
        let rec min node cont =
            match node with
            | PE -> PT(1, PE, Node.create pcStart pcLength, PE) |> cont
            | PT(h, l, v, r) ->
                min l (fun l' ->
                    let v = v.AddLeft pcLength
                    PT(h, l', v, r)
                    |> skew |> split |> cont
                )
        min tree topLevelCont

    let private insMax pcStart pcLength tree =
        let rec max node cont =
            match node with
            | PE -> PT(1, PE, Node.create pcStart pcLength, PE) |> cont
            | PT(h, l, v, r) ->
                max r (fun r' ->
                    let v = v.AddRight pcLength
                    PT(h, l, v, r')
                    |> skew |> split |> cont
                )
        max tree topLevelCont

    let inline isConsecutive (v: PieceNode) pcStart =
        v.Start + v.Length = pcStart

    let insert insIndex pcStart pcLength tree =
        let rec ins curIndex node = 
            match node with
            | PE -> PT(1, PE, Node.create pcStart pcLength, PE)
            | PT(h, l, v, r) when insIndex < curIndex ->
                let nextIndex = curIndex - nLength l - sizeRight l
                let v' = v.AddLeft pcLength
                PT(h, ins nextIndex l, v', r) |> skew |> split
            | PT(h, l, v, r) when insIndex > curIndex + v.Length ->
                let nextIndex = curIndex + sizeLeft r
                let v' = v.AddRight pcLength
                PT(h, l, v', ins nextIndex r) |> skew |> split
            | PT(h, l, v, r) when insIndex = curIndex ->
                let v' = v.AddLeft pcLength
                PT(h, insMax pcStart pcLength l, v', r) |> skew |> split
            | PT(h, l, v, r) when insIndex = curIndex + v.Length && isConsecutive v pcStart ->
                let v' = { v with Length = v.Length + pcLength }
                PT(h, l, v', r) 
            | PT(h, l, v, r) when insIndex = curIndex + v.Length ->
                let v' = v.AddRight pcLength
                PT(h, l, v', insMin pcStart pcLength r) |> skew |> split
            | PT(h, l, v, r) ->
                let difference = insIndex - curIndex
                let rStart = v.Start + difference
                let l' = insMax v.Start difference l
                let r' = insMin rStart (v.Length - difference) r
                let v' = { v with Start = pcStart; Length = pcLength; }
                PT(h, l', v', r') |> skew |> split

        ins (sizeLeft tree) tree

    (* Repeated if-statements used in both delete and substring. *)
    let inline private inRange start curIndex finish nodeEndIndex =
        start <= curIndex && finish >= nodeEndIndex

    let inline private startIsInRange start curIndex finish nodeEndIndex =
        start <= curIndex && finish < nodeEndIndex && curIndex < finish

    let inline private endIsInRange start curIndex finish nodeEndIndex =
        start > curIndex && finish >= nodeEndIndex && start <= nodeEndIndex

    let inline private middleIsInRange start curIndex finish nodeEndIndex =
        start >= curIndex && finish <= nodeEndIndex

    let substring (start: int) (length: int) table =
        let finish = start + length
        let rec sub curIndex node acc =
            match node with
            | PE -> acc
            | PT(h, l, v, r) ->
                let left =
                    if start < curIndex
                    then sub (curIndex - nLength l - sizeRight l) l acc
                    else acc

                let nodeEndIndex = curIndex + v.Length
                let middle = 
                    if inRange start curIndex finish nodeEndIndex then
                        left + PieceLogic.text v table
                    elif startIsInRange start curIndex finish nodeEndIndex then
                        left + PieceLogic.textAtStart curIndex finish v table
                    elif endIsInRange start curIndex finish nodeEndIndex then
                        left + PieceLogic.textAtEnd curIndex start v table
                    elif middleIsInRange start curIndex finish nodeEndIndex then
                        left + PieceLogic.textInRange curIndex start finish v table
                    else
                        left

                if finish > nodeEndIndex
                then sub (nodeEndIndex + sizeLeft r) r middle
                else middle

        sub (sizeLeft table.Tree) table.Tree ""


    let delete start length tree =
        let finish = start + length
        let rec del curIndex node =
            match node with
            | PE -> PE
            | PT(h, l, v, r) as node ->
                let left =
                    if start < curIndex
                    then del (curIndex - nLength l - sizeRight l) l
                    else l
                let nodeEndIndex: int = curIndex + v.Length
                let right =
                    if finish > nodeEndIndex
                    then del (nodeEndIndex + sizeLeft r) r
                    else r
                
                if inRange start curIndex finish nodeEndIndex then
                    if left = PE
                    then right
                    else 
                        let (newLeft, newVal) = splitMax left
                        let v' = {newVal with LeftIdx = size newLeft; RightIdx = size right; }
                        PT(h, newLeft, v', right) |> adjust
                elif startIsInRange start curIndex finish nodeEndIndex then
                    let v' = PieceLogic.deleteAtStart curIndex finish v
                    let v' = { v' with LeftIdx = size left; RightIdx = size right; }
                    PT(h, left, v', right) |> skew |> split
                elif endIsInRange start curIndex finish nodeEndIndex then
                    let v' = PieceLogic.deleteAtEnd curIndex start v
                    let v' = {v' with LeftIdx = size left; RightIdx = size right }
                    PT(h, left, v', right) |> adjust
                elif middleIsInRange start curIndex finish nodeEndIndex then
                    let (p1, p2Start, p2Length) = PieceLogic.deleteInRange curIndex start finish v
                    let newRight = insMin p2Start p2Length right
                    let v' = {p1 with LeftIdx = size left; RightIdx = size right }
                    PT(h, left, v', newRight) |> skew |> split
                else
                    let v' = {v with LeftIdx = size left; RightIdx = size right }
                    PT(h, left, v', right) |> adjust
                
        del (sizeLeft tree) tree 