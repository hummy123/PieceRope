namespace PieceTree

open Buffer.Types

module Types =
    type PieceNode = {
        Data: int array;
        // LeftLn: int; <- Add this later
        // RightLn: int; <- Add this later
    } with
        member inline this.Start = this.Data[0]
        member inline this.Length = this.Data[1]
        member inline this.LeftIdx = this.Data[2]
        member inline this.RightIdx = this.Data[3]


    type PieceTree =
        | PE
        | PT of int * PieceTree * PieceNode * PieceTree

    type PieceRope = {
        Buffer: BufferTree
        Tree: PieceTree;
    }
