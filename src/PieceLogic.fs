namespace PieceTree

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

    let inline deleteLinesInRange p1Length p2Start lines =
        let p1Lines = ResizeArray()
        let p2Lines = ResizeArray()
        for i in lines do
            if i < p1Length
            then p1Lines.Add i
            elif i >= p2Start
            then p2Lines.Add i
        p1Lines.ToArray(), p2Lines.ToArray()

    let inline deleteInRange curIndex start finish (piece: PieceNode) =
        (* p1 retains metadata and p2 is leaf *)
        let finishDifference = finish - curIndex
        let p1Length = start - curIndex
        let p2Start = finishDifference + piece.Start
        let (p1Lines, p2Lines) = deleteLinesInRange (p1Length + piece.Start) p2Start piece.Lines
        let p1 = {piece with Length = p1Length; Lines = p1Lines}

        let p2Length = piece.Length - finishDifference

        (p1, p2Start, p2Length, p2Lines)

    let inline deleteAtStart curIndex finish piece =
        let difference = finish - curIndex
        let newStart = piece.Start + difference
        let newLength = piece.Length - difference
        let newLines = Array.filter (fun x -> x >= difference) piece.Lines
        { piece with Start = newStart; Length = newLength; Lines = newLines }

    let inline deleteAtEnd curIndex start piece =
        let length = start - curIndex
        let lines = Array.filter (fun x -> x <= length) piece.Lines
        { piece with Length = length; Lines = lines }

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