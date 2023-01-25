﻿namespace PieceTree

open Buffer.Tree
open PieceTree.Types
open PieceTree.Node

module PieceLogic =
    /// Returns two arrays containing the lines in a PieceNode, split at a specific position.
    /// Intended to be used when we insert into a piece.
    let inline splitLines rstart lines =
        let arrLeft = ResizeArray()
        let arrRight = ResizeArray()
        for i in lines do
            if i < rstart (* not sure if it should be < or <= *)
            then arrLeft.Add i
            else arrRight.Add i

        arrLeft.ToArray(), arrRight.ToArray()

    let inline deleteLinesInRange p1Length p2Start (lines: int array) =
        let p1Lines = 
            match Array.tryFindIndex (fun x -> x > p1Length) lines with
            | Some x -> lines[.. x - 1 ]
            | None -> [||]

        let p2Lines =
            match Array.tryFindIndex (fun x -> x >= p2Start) lines with
            | Some x -> lines[x..]
            | None -> [||]
        
        lines[..start], p2Lines

    let inline deleteInRange curIndex start finish (piece: PieceNode) =
        (* p1 retains metadata and p2 is leaf *)
        let finishDifference = finish - curIndex
        let p1Length = start - curIndex
        let p2Start = finishDifference + piece.Start
        let (p1Lines, p2Lines) = deleteLinesInRange (p1Length + piece.Start) p2Start piece.Lines

        let p2Length = piece.Length - finishDifference

        (p1Length, p1Lines, p2Start, p2Length, p2Lines)

    let inline deleteAtStart curIndex finish piece =
        let difference = finish - curIndex
        let newStart = piece.Start + difference
        let newLength = piece.Length - difference
        let newLines = Array.filter (fun x -> x >= difference) piece.Lines
        (newStart, newLength, newLines)

    let inline deleteAtEnd curIndex start piece =
        let length = start - curIndex
        let lines = Array.filter (fun x -> x <= length) piece.Lines
        (length, lines)

    let inline text piece table =
        substring piece.Start piece.Length table.Buffer

    let inline textInRange curIndex start finish piece table =
        let textStart = start - curIndex + piece.Start
        let textLength = finish - curIndex + piece.Start - textStart
        substring textStart textLength table.Buffer

    let inline textAtStart curIndex finish piece table =
        let textLength = finish - curIndex
        substring piece.Start textLength table.Buffer

    let inline textAtEnd curIndex start piece table =
        let textStart = start - curIndex + piece.Start
        let textLength = piece.Start + piece.Length - textStart
        substring textStart textLength table.Buffer

    /// Returns a substring at the provided start and length.
    let inline atStartAndLength start length table =
        substring start length table.Buffer