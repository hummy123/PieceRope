namespace AppendRope

module AppendRopeTypes =
    type RopeNode = {
        mutable String: string;
        mutable LeftIdx: int;
        mutable RightIdx: int;
    }

    type RopeTree =
        | E
        | T of int * RopeTree * RopeNode * RopeTree

    type AppendRope = {
        mutable Tree: RopeTree;
        mutable TextLength: int;
        mutable LineCount: int;
    }

    (* When we are appending, if the last node contains a string less than this,
     * then we concatenate; else, we create a new node. *)
    [<Literal>]
    let TargetNodeLength = 1024
