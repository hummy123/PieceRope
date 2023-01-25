namespace PieceTree 

open PieceTree.Types

module Data = 
    let inline lineArrLength (lines: int array option) =
        match lines with
        | Some x -> x.Length
        | None -> 0

    let inline nLength node = 
        match node with
        | PE -> 0
        | PT(_, _, v, _) -> v.Length

    let inline nLines node =
        match node with
        | PE -> 0
        | PT(h, l, v, r) -> lineArrLength v.Lines

    let inline size node =
        match node with
        | PE -> 0
        | PT(_, _, v, _) -> v.LeftIdx + v.RightIdx + v.Length

    let inline idxLnSize node =
        match node with
        | PE -> 0, 0
        | PT(h, l, v, r) -> v.LeftIdx + v.RightIdx + v.Length,
                            v.LeftLn + v.RightLn + lineArrLength v.Lines

    let inline stringLength node =
        match node with
        | PE -> 0
        | PT(_, l, v, _) -> v.Length

    let inline sizeLeft node = 
        match node with 
        | PE -> 0
        | PT(_, _, v, _) -> v.LeftIdx

    let inline sizeRight node = 
        match node with 
        | PE -> 0
        | PT(_, _, v, _) -> v.RightIdx

    let inline linesLeft node =
        match node with
        | PE -> 0
        | PT(h, l, v, r) -> v.LeftLn

    let inline linesRight node =
        match node with
        | PE -> 0
        | PT(h, l, v, r) -> v.RightLn