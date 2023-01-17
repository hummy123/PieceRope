namespace PieceTree 

open PieceTree.Types

module Data = 
    let inline pcLength node = 
        match node with
        | PE -> 0
        | PT(_, _, v, _) -> v.Length

    let inline size node =
        match node with
        | PE -> 0
        | PT(_, _, v, _) -> v.LeftIdx + v.RightIdx + v.Length

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
