module RopeeGetLineTests

open System
open Xunit
open PieceRope

[<Fact>]
let ``Rope.GetLine returns line we inserted`` () =
    let rope = PieceRope.create "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n"
    for i in [0..9] do
        let line = PieceRope.getLine i rope
        Assert.Equal(sprintf "%i\n" i, line)
