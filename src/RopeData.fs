namespace AppendRope 

open AppendRopeTypes

module RopeData = 
    let inline size node =
        match node with
        | E -> 0
        | T(_, _, v, _) -> v.LeftIdx + v.RightIdx + v.String.Length

    let inline stringLength node =
        match node with
        | E -> 0
        | T(_, l, v, _) -> v.String.Length

    let inline sizeLeft node = 
        match node with 
        | E -> 0
        | T(_, _, v, _) -> v.LeftIdx

    let inline sizeRight node = 
        match node with 
        | E -> 0
        | T(_, _, v, _) -> v.RightIdx
