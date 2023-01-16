namespace AppendRope

module AppendRopeTypes =
    type RopeNode = {
        String: string;
        LeftIdx: int;
        RightIdx: int;
    }

    type RopeTree =
        | E
        | T of int * RopeTree * RopeNode * RopeTree

    type AppendRope = {
        Tree: RopeTree;
        TextLength: int;
        LineCount: int;
    }
