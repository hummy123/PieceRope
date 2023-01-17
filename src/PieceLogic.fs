namespace PieceTree

open Buffer.Tree
open PieceTree.Types
open PieceTree.Node

module PieceLogic =
    /// Merges two pieces into one.
    /// Expects to be called only when pieces are consecutive. 
    let inline merge (a: PieceNode) (b: PieceNode) =
        { a with Length = a.Length + b.Length }

    /// Split operation that returns three pieces.
    /// Correct usage of this method assumes that Piece a starts before and ends after Piece b.
    let inline split (a: PieceNode) (difference: int) =
        (* p1 and p3 are both leaves with no child nodes. 
          The piece we are inserting should take node "A"s index metadata. *)
        let p1Length = a.Start + difference
        let p1 = Node.create a.Start p1Length

        let p2Finish = a.Start + a.Length - difference
        let p3 = Node.create p1Length (p2Finish)
        (p1, p3)

    let inline deleteInRange curIndex start finish (piece: PieceNode) =
        (* p1 retains metadata and p2 is leaf *)
        let p1Length = start - curIndex
        let p1 = {piece with Length = p1Length}

        let p2Start = finish - curIndex + piece.Start
        let p2Finish = piece.Start + piece.Length - p2Start
        let p2 = Node.create p2Start p2Finish

        (p1, p2)

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
