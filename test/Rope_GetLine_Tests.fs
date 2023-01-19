module RopeeGetLineTests

open System
open Xunit
open PieceRope
open PieceRope.PieceRope

[<Fact>]
let ``Rope.GetLine returns line we inserted`` () =
    let rope = PieceRope.create "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n"
    for i in [0..9] do
        let line = PieceRope.getLine i rope
        Assert.Equal(sprintf "%i\n" i, line)
    let line = PieceRope.getLine 10 rope
    Assert.Equal("\n", line)

[<Fact>]
let ``Rope.GetLine returns whole string when we delete line break at middle.`` () =
    let rope = PieceRope.create "abcde\nfghij"
    Assert.Equal("abcde\n", rope.GetLine 0)
    Assert.Equal("fghij", rope.GetLine 1)

    let rope = rope.Delete(5, 1)
    Assert.Equal("abcdefghij", rope.GetLine 0)
