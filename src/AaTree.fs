namespace AppendRope

open AppendRopeTypes
open RopeNode
open RopeData

module internal AaTree =
    let inline sngl node =
        match node with
        | E -> false
        | T(_, _, _, E) -> true
        | T(lvx, _, _, T(lvy, _, _, _)) -> lvx > lvy

    let inline lvl node =
        match node with
        | E -> 0
        | T(lvt, _, _, _) -> lvt

    let inline skew node =
        match node with
        | T(lvx, T(lvy, a, ky, b), kx, c) as t when lvx = lvy ->
            let kx = kx.SetIdx (size b) (size c)
            let innerNode =  T(lvx, b, kx, c)
            let ky = ky.SetIdx (size a) (size innerNode)
            T(lvx, a, ky, innerNode)
        | t -> t

    let inline split node =
        match node with
        | T(lvx, a, kx, T(lvy, b, ky, T(lvz, c, kz, d))) as t when lvx = lvy && lvy = lvz -> 
            let right = T(lvx, c, kz, d)
            let kx = kx.SetIdx (size a) (size b)
            let left = T(lvx, a, kx, b)
            let ky = ky.SetIdx (size left) (size right)
            T(lvx + 1, left, ky, right)
        | t -> t
