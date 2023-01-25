namespace Buffer 

open Types

module Data = 
    let inline size node =
        match node with
        | BE -> 0
        | BT(_, _, lm, v, rm, _) -> lm + rm + v.Length

    let inline stringLength node =
        match node with
        | BE -> 0
        | BT(_, _, _, v, _, _) -> v.Length

    let inline sizeLeft node = 
        match node with 
        | BE -> 0
        | BT(_, _, lm, _, _, _) -> lm

    let inline sizeRight node = 
        match node with 
        | BE -> 0
        | BT(_, _, _, _, rm, _) -> rm
