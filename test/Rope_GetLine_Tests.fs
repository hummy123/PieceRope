module RopeeGetLineTests

open System
open Xunit
open PieceRope
open PieceRope.PieceRope

(* Get line under create tests. *)
[<Fact>]
let ``Rope.GetLine returns line we inserted`` () =
    let rope = PieceRope.create "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n"
    for i in [0..9] do
        let line = PieceRope.getLine i rope
        Assert.Equal(sprintf "%i\n" i, line)
    let line = PieceRope.getLine 10 rope
    Assert.Equal("", line)

(* Get line under insert tests. *)


(* Get line under delete tests. *)
[<Fact>]
let ``Rope.GetLine returns whole string when we delete line break at middle.`` () =
    let rope = PieceRope.create "abcde\nfghij"
    Assert.Equal("abcde\n", rope.GetLine 0)
    Assert.Equal("fghij", rope.GetLine 1)

    let rope = rope.Delete(5, 1)
    Assert.Equal("abcdefghij", rope.GetLine 0)

[<Fact>]
let ``Rope.GetLine returns correct segments when we delete line breaks in complex string`` () =
    let rope = PieceRope.create "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
    
    // Delete first line break and see if we can get expected string from result.
    let rope = rope.Delete(11, 1)
    Assert.Equal("Lorem ipsumdolor sit amet,\n", rope.GetLine 0)
    Assert.Equal("consectetur\n", rope.GetLine 1)

    // Delete third line break.
    let rope = rope.Delete(38, 1)
    Assert.Equal("Lorem ipsumdolor sit amet,\n", rope.GetLine 0)
    Assert.Equal("consecteturadipiscing elit. \n", rope.GetLine 1)

    // Delete fifth line break
    let rope = rope.Delete(71, 1)
    Assert.Equal("Lorem ipsumdolor sit amet,\n", rope.GetLine 0)
    Assert.Equal("consecteturadipiscing elit. \n", rope.GetLine 1)
    Assert.Equal("Aenean ornare, lacus vitae \n", rope.GetLine 2)


[<Fact>]
let ``Rope.GetLine returns correct segments when we delete multiple line breaks at first half`` () =
    let str = "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
    let rope = PieceRope.create str

    // delete "\ndolor sit amet,\n"
    let rope = rope.Delete(11, 17)
    let str = str.Remove(11, 17)

    // check text is same after deletion (side-check to ensure consistency)
    Assert.Equal(str, rope.Text())

    Assert.Equal("Lorem ipsumconsectetur\n", rope.GetLine 0)
    Assert.Equal("adipiscing elit. \n", rope.GetLine 1)

    // delete "\nlacus vitae \ntempor pretium,\n"
    let rope = rope.Delete(57, 29)
    let str = str.Remove(57, 29)
    Assert.Equal(str, rope.Text())

    // previos assertions to check they still work
    Assert.Equal("Lorem ipsumconsectetur\n", rope.GetLine 0)
    Assert.Equal("adipiscing elit. \n", rope.GetLine 1)

    // current assertion
    Assert.Equal("Aenean ornare, \n", rope.GetLine 2)
    Assert.Equal("leo nulla\n", rope.GetLine 3)

    // assertions for lines after to check they still work as expected
    Assert.Equal("sollicitudin elit,\n", rope.GetLine 4)
    Assert.Equal("in ultrices mi dui et\n", rope.GetLine 5)
    Assert.Equal("ipsum. Cras condimentum\n", rope.GetLine 6)
    Assert.Equal("purus in metus \n", rope.GetLine 7)
    Assert.Equal("sodales tincidunt. Praesent", rope.GetLine 8)

[<Fact>]
let ``Rope.GetLine returns correct segments when we delete multiple line breaks at last half`` () =
    let rope = PieceRope.create "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"

    // delete "\npurus in metus \n"
    let rope = rope.Delete(177, 17)
    Assert.Equal("ipsum. Cras condimentumsodales tincidunt. Praesent", rope.GetLine 10)

    // delete " elit,\nin "
    let rope = rope.Delete(123, 9)
    Assert.Equal("sollicitudin ultrices mi dui et\n", rope.GetLine 8)

    // delete "\ntempor pretium,\nleo nulla"
    let rope = rope.Delete(83, 25)

    // assert that all lines are as expected
    Assert.Equal("Lorem ipsum\n", rope.GetLine 0)
    Assert.Equal("dolor sit amet,\n", rope.GetLine 1)
    Assert.Equal("consectetur\n", rope.GetLine 2)
    Assert.Equal("adipiscing elit. \n", rope.GetLine 3)
    Assert.Equal("Aenean ornare, \n", rope.GetLine 4)
    Assert.Equal("lacus vitulla\n", rope.GetLine 5)
    Assert.Equal("sollicitudin ultrices mi dui et\n", rope.GetLine 6)
    Assert.Equal("ipsum. Cras condimentumsodales tincidunt. Praesent", rope.GetLine 7)

[<Fact>]
let ``Rope.GetLine returns correct segments when we delete multiple line breaks in middle`` () =
    let str = "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
    let rope = PieceRope.create str

    // delete "Aenean ornare, \n"
    let rope = rope.Delete(58, 16)
    let str = str.Remove(58, 16)
    Assert.Equal(str, rope.Text())

    // delete "tempor pretium,\nleo nulla\n"
    let rope = rope.Delete(71, 26)
    let str = str.Remove(71, 26)
    Assert.Equal(str, rope.Text())

    //delete "dolor sit amet,\n"
    let rope = rope.Delete(12, 16)
    let str = str.Remove(12, 16)
    Assert.Equal(str, rope.Text())
    
    // test we retrieve all lines as expected
    Assert.Equal("Lorem ipsum\n", rope.GetLine 0)
    Assert.Equal("consectetur\n", rope.GetLine 1)
    Assert.Equal("adipiscing elit. \n", rope.GetLine 2)
    Assert.Equal("lacus vitae \n", rope.GetLine 3)
    Assert.Equal("sollicitudin elit,\n", rope.GetLine 4)
    Assert.Equal("in ultrices mi dui et\n", rope.GetLine 5)
    Assert.Equal("ipsum. Cras condimentum\n", rope.GetLine 6)
    Assert.Equal("purus in metus \n", rope.GetLine 7)
    Assert.Equal("sodales tincidunt. Praesent", rope.GetLine 8)