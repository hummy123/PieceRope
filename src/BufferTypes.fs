namespace Buffer

module Types =
    type BufferNode = {
        String: string;
        LeftIdx: int;
        RightIdx: int;
    }

    type BufferTree =
        | BE
        | BT of int * BufferTree * int * string * int * BufferTree
