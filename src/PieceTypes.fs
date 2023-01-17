namespace PieceTree

open Buffer.Types

module Types =
    type PieceNode = {
        Start: int;
        Length: int;
        LeftIdx: int;
        RightIdx: int;
        // LeftLn: int; <- Add this later
        // RightLn: int; <- Add this later
    }

    type PieceTree =
        | PE
        | PT of int * PieceTree * PieceNode * PieceTree

    type PieceRope = {
        Buffer: BufferTree
        Tree: PieceTree;
    }
