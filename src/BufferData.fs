namespace Buffer 

open Types

module Data = 
    let inline size node =
        match node with
        | BE -> 0
        | BT(_, _, v, _) -> v.LeftIdx + v.RightIdx + v.String.Length

    let inline stringLength node =
        match node with
        | BE -> 0
        | BT(_, l, v, _) -> v.String.Length

    let inline sizeLeft node = 
        match node with 
        | BE -> 0
        | BT(_, _, v, _) -> v.LeftIdx

    let inline sizeRight node = 
        match node with 
        | BE -> 0
        | BT(_, _, v, _) -> v.RightIdx
