﻿namespace PieceTree

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
            let kx = kx.SetData (idxLnSize b) (idxLnSize c)
            let innerNode =  PT(lvx, b, kx, c)
            let ky = ky.SetData (idxLnSize a) (idxLnSize innerNode)
            PT(lvx, a, ky, innerNode)
        | t -> t

    let inline split node =
        match node with
        | PT(lvx, a, kx, PT(lvy, b, ky, PT(lvz, c, kz, d))) as t when lvx = lvy && lvy = lvz -> 
            let right = PT(lvx, c, kz, d)
            let kx = kx.SetData (idxLnSize a) (idxLnSize b)
            let left = PT(lvx, a, kx, b)
            let ky = ky.SetData (idxLnSize left) (idxLnSize right)
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
            let kl = kl.SetData (idxLnSize a) (idxLnSize lb)
            let leftNode = PT(lv1, a, kl, lb)
            let kt = kt.SetData (idxLnSize rb) (idxLnSize rt)
            let rightNode = PT(lvt - 1, rb, kt, rt)
            let kb = kb.SetData (idxLnSize leftNode) (idxLnSize rightNode)
            PT(lvb + 1, leftNode, kb, rightNode)
        | PT(lvt, lt, kt, rt) when lvl rt < lvt -> 
            PT(lvt - 1, lt, kt, rt) |> split
        | PT(lvt, lt, kt, PT(lvr, (PT(lva, c, ka, d) as a), kr, b)) ->
            let kt = kt.SetData (idxLnSize lt) (idxLnSize c)
            let leftNode = PT(lvt - 1, lt, kt, c)
            let kr = kr.SetData (idxLnSize d) (idxLnSize b)
            let rightNode = PT(nlvl a, d, kr, b) |> split
            let ka = ka.SetData (idxLnSize leftNode) (idxLnSize rightNode)
            PT(lva + 1, leftNode, ka, rightNode)
        | _ -> node

    let rec splitMax =
        function
        | PT(_, l, v, PE) -> 
            let (lSize, lLines) = idxLnSize l
            let v = 
                { v with 
                    LeftIdx = lSize; 
                    LeftLn = lLines;
                    RightIdx = 0;
                    RightLn = 0; }
            l, v
        | PT(h, l, v, r) -> 
            match splitMax r with
            | r', b -> 
                let (r'Size, r'Lines) = idxLnSize r'
                let v' = { v with RightIdx = r'Size; RightLn = r'Lines; }
                let newLeft = PT(h, l, v', r') |> adjust
                let (treeSize, treeLines) = idxLnSize newLeft
                let b' = { b with LeftIdx = treeSize; LeftLn = treeLines }
                newLeft, b'
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
open PieceLogic

(* Contains core PieceTree logic. *)
module PieceTree =
    let empty = PE

    let text piecerope =
        fold (fun acc pc ->
            let text = Buffer.Tree.substring pc.Start pc.Length piecerope.Buffer
            acc + text) "" piecerope.Tree

    /// Used for CPS.
    let inline topLevelCont t = t

    let private insMin pcStart pcLength pcLines tree =
        let rec min node cont =
            match node with
            | PE -> PT(1, PE, Node.create pcStart pcLength pcLines, PE) |> cont
            | PT(h, l, v, r) ->
                min l (fun l' ->
                    let v = v.AddLeft pcLength (lineArrLength pcLines)
                    PT(h, l', v, r)
                    |> skew |> split |> cont
                )
        min tree topLevelCont

    let private insMax pcStart pcLength pcLines tree =
        let rec max node cont =
            match node with
            | PE -> PT(1, PE, Node.create pcStart pcLength pcLines, PE) |> cont
            | PT(h, l, v, r) ->
                max r (fun r' ->
                    let v = v.AddRight pcLength (lineArrLength pcLines)
                    PT(h, l, v, r')
                    |> skew |> split |> cont
                )
        max tree topLevelCont

    let inline isConsecutive (v: PieceNode) pcStart =
        v.Start + v.Length = pcStart

    let insert insIndex pcStart pcLength pcLines pcLineCount tree =
        let rec ins curIndex node cont = 
            match node with
            | PE -> PT(1, PE, Node.create pcStart pcLength pcLines, PE) |> cont
            | PT(h, l, v, r) when insIndex < curIndex ->
                let nextIndex = curIndex - nLength l - sizeRight l
                let v' = v.AddLeft pcLength pcLineCount
                ins nextIndex l (fun l' ->
                    PT(h, l', v', r) |> skew |> split |> cont
                )
            | PT(h, l, v, r) when insIndex > curIndex + v.Length ->
                let nextIndex = curIndex + v.Length + sizeLeft r
                let v' = v.AddRight pcLength pcLineCount
                ins nextIndex r (fun r' ->
                    PT(h, l, v', r') |> skew |> split |> cont
                )
            | PT(h, l, v, r) when insIndex = curIndex ->
                let v' = v.AddLeft pcLength pcLineCount
                let l' = insMax pcStart pcLength pcLines l
                PT(h, l', v', r) |> skew |> split |> cont
            | PT(h, l, v, r) when insIndex = curIndex + v.Length && isConsecutive v pcStart ->
                let v'Lines =
                    match v.Lines, pcLines with
                    | Some vln, Some pcln -> Some <| Array.append vln pcln
                    | (Some _) as ln, _ -> ln
                    | _, ((Some _) as ln) -> ln
                    | None, None -> None

                let v' = { v with Length = v.Length + pcLength; Lines = v'Lines }
                PT(h, l, v', r) |> cont
            | PT(h, l, v, r) when insIndex = curIndex + v.Length ->
                let v' = v.AddRight pcLength pcLineCount
                let r' = insMin pcStart pcLength pcLines r
                PT(h, l, v', r') |> skew |> split |> cont
            | PT(h, l, v, r) ->
                let difference = insIndex - curIndex
                let rStart = v.Start + difference
                let rLength = v.Length - difference

                let (leftLines, rightLines) = 
                    match v.Lines with
                    | Some x -> 
                        let left, right = splitLines rStart x
                        Some(left), Some(right)
                    | None -> None, None

                let l' = insMax v.Start difference leftLines l
                let r' = insMin rStart rLength rightLines r
                let v' = { v with 
                            Start = pcStart; 
                            Length = pcLength; 
                            Lines = pcLines;
                            LeftIdx = v.LeftIdx + difference;
                            LeftLn = v.LeftLn + lineArrLength leftLines;
                            RightIdx = v.RightIdx + rLength; 
                            RightLn = v.RightLn + lineArrLength rightLines; }
                PT(h, l', v', r') |> skew |> split |> cont

        ins (sizeLeft tree) tree topLevelCont

    (* Repeated if-statements used in both delete and substring. *)
    let inline private inRange start curIndex finish nodeEndIndex =
        start <= curIndex && finish >= nodeEndIndex

    let inline private startIsInRange start curIndex finish nodeEndIndex =
        start <= curIndex && finish < nodeEndIndex && curIndex < finish

    let inline private endIsInRange start curIndex finish nodeEndIndex =
        start > curIndex && finish >= nodeEndIndex && start <= nodeEndIndex

    let inline private middleIsInRange start curIndex finish nodeEndIndex =
        start >= curIndex && finish <= nodeEndIndex


    let delete start length tree =
        let finish = start + length
        let rec del curIndex node =
            match node with
            | PE -> PE
            | PT(h, l, v, r) ->
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
                        let v' = newVal.SetData (idxLnSize newLeft) (idxLnSize right) 
                        PT(h, newLeft, v', right) |> adjust
                elif startIsInRange start curIndex finish nodeEndIndex then
                    let (newStart, newLength, newLines) = PieceLogic.deleteAtStart curIndex finish v
                    let (leftIdx, leftLns) = idxLnSize left
                    let (rightIdx, rightLns) = idxLnSize right
                    let v' = { v with 
                                Start = newStart; 
                                Length = newLength;
                                Lines = newLines;
                                LeftIdx = leftIdx;
                                LeftLn = leftLns;
                                RightIdx = rightIdx;
                                RightLn = rightLns; }                    
                    PT(h, left, v', right) |> skew |> split
                elif endIsInRange start curIndex finish nodeEndIndex then
                    let (length, lines) = PieceLogic.deleteAtEnd curIndex start v
                    let (leftIdx, leftLns) = idxLnSize left
                    let (rightIdx, rightLns) = idxLnSize right
                    let v' = { v with
                                Length = length;
                                Lines = lines; 
                                LeftIdx = leftIdx;
                                LeftLn = leftLns; 
                                RightIdx = rightIdx;
                                RightLn = rightLns; }
                    PT(h, left, v', right) |> adjust
                elif middleIsInRange start curIndex finish nodeEndIndex then
                    let (p1Length, p1Lines, p2Start, p2Length, p2Lines)
                        = PieceLogic.deleteInRange curIndex start finish v
                    let newRight = insMin p2Start p2Length p2Lines right
                    let (leftIdx, leftLns) = idxLnSize left
                    let (rightIdx, rightLns) = idxLnSize newRight
                    let v' = { v with
                                Length = p1Length;
                                Lines = p1Lines; 
                                LeftIdx = leftIdx;
                                LeftLn = leftLns;
                                RightIdx = rightIdx;
                                RightLn = rightLns; }
                    PT(h, left, v', newRight) |> skew |> split
                else
                    let v' = v.SetData (idxLnSize left) (idxLnSize right) 
                    PT(h, left, v', right) |> adjust
                
        del (sizeLeft tree) tree 

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

    (* Delete/substring if-statements adapted to work with lines. *)
    let inline private lineInRange nodeStartLine searchLine nodeEndLine =
        nodeStartLine = searchLine && searchLine = nodeEndLine

    let inline private startIsInLine nodeStartLine searchLine nodeEndLine =
        nodeStartLine = searchLine && searchLine < nodeEndLine

    let inline private endIsInLine nodeStartLine searchLine nodeEndLine =
        nodeStartLine < searchLine && searchLine = nodeEndLine

    let inline private middleIsInLine nodeStartLine searchLine nodeEndLine =
        nodeStartLine < searchLine && nodeEndLine > searchLine

    let getLine line table =
        let rec get curLine node acc =
            match node with
            | PE -> acc
            | PT(h, l, v, r) ->
                let left = 
                    if line <= curLine
                    then get (curLine - nLines l - linesRight l) l acc
                    else acc

                let nodeEndLine = curLine + (lineArrLength v.Lines)
                let middle =
                    if lineInRange curLine line nodeEndLine then
                        left + PieceLogic.text v table
                    elif startIsInLine curLine line nodeEndLine then
                        (* + 1 gives us \n in string and - v.Start takes us to piece offset *)
                        let (Some vlns) = v.Lines
                        let length = vlns[0] + 1 - v.Start
                        left + PieceLogic.atStartAndLength v.Start length table
                    elif endIsInLine curLine line nodeEndLine then
                        let (Some vlns) = v.Lines
                        let start = vlns[vlns.Length - 1] + 1
                        let length = v.Length - start + v.Start
                        left + PieceLogic.atStartAndLength start length table
                    elif middleIsInLine curLine line nodeEndLine then
                        let (Some vlns) = v.Lines
                        let lineDifference = line - curLine
                        let lineStart = vlns[lineDifference - 1] + 1
                        let lineLength = vlns[lineDifference] - lineStart + 1
                        left + PieceLogic.atStartAndLength lineStart lineLength table
                    else
                        left

                if line >= nodeEndLine
                then get (nodeEndLine + linesLeft r) r middle
                else middle

        get (linesLeft table.Tree) table.Tree ""