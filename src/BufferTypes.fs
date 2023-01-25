namespace Buffer

module Types =
    type BufferTree =
        | BE
        | BT of int * BufferTree * int * string * int * BufferTree

    [<Literal>]
    let TargetNodeSize = 512