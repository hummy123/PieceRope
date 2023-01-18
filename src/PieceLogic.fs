namespace PieceTree

open Buffer.Tree
open PieceTree.Types
open PieceTree.Node

module PieceLogic =
    let inline deleteInRange curIndex start finish (piece: PieceNode) =
        (* p1 retains metadata and p2 is leaf *)
        let p1Length = start - curIndex
        let p1 = {piece with Length = p1Length}

        let p2Start = finish - curIndex + piece.Start
        let p2Length = piece.Length - p2Start

        (p1, p2Start, p2Length)

    let inline deleteAtStart curIndex finish piece =
        let difference = finish - curIndex
        let newStart = piece.Start + difference
        let newLength = piece.Length - difference
        { piece with Start = newStart; Length = newLength; }

    let inline deleteAtEnd curIndex start piece =
        { piece with Length = start - curIndex }

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
        let textLength = start - curIndex
        let textStart = textLength + piece.Start
        substring textStart textLength table.Buffer
