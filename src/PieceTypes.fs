namespace PieceTree

open Buffer.Types

module Types =
    type PieceNode = {
        Start: int;
        Length: int;
        LeftIdx: int;
        RightIdx: int;
        LeftLn: int;
        RightLn: int;
        Lines: int array option;
    }

    type PieceTree =
        | PE
        | PT of int * PieceTree * PieceNode * PieceTree

    type PieceRope = {
        Buffer: BufferTree
        Tree: PieceTree;
    }
