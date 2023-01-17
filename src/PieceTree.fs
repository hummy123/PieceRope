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
            l, v, 0
        | PT(h, l, v, r) -> 
            match splitMax r with
            | r', b, lns -> 
                let v' = { v with RightIdx = size r' }
                let tree = PT(h, l, v', r') |> adjust
                let b' = { b with RightIdx = size tree }
                tree, b', lns
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

(* Contains core PieceTree logic. *)
module PieceTree =
    let d = 0