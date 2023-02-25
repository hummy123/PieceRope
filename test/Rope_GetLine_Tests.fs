module RopeeGetLineTests

open System
open Xunit
open HumzApps.TextDocument

(* Get line under create tests. *)
[<Fact>]
let ``Rope.GetLine returns line we inserted`` () =
    let rope = TextDocument.create "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n"
    for i in [0..9] do
        let line = TextDocument.getLine i rope
        Assert.Equal(sprintf "%i\n" i, line)
    let line = TextDocument.getLine 10 rope
    Assert.Equal("", line)

(* Get line under insert tests. *)
[<Fact>]
let ``Rope.GetLine returns two strings when we insert line break at middle.`` () = 
    let str = "abcdefghij"
    let rope = 
      TextDocument.create str
      |> TextDocument.insert 4 "\n"

    let ropeText = TextDocument.text rope
    let ropeLine0 = TextDocument.getLine 0 rope
    let ropeLine1 = TextDocument.getLine 1 rope
    
    let str = str.Insert(4, "\n")
    let strLines = str.Split("\n")
    
    Assert.Equal(str, ropeText)
    Assert.Equal(strLines[0] + "\n", ropeLine0)
    Assert.Equal(strLines[1], ropeLine1)

[<Fact>]
let ``Rope.GetLine splits lines correctly when we insert into middle of piece`` () =
    // Initial data
    let initString = "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
    let initRope = TextDocument.create initString

    let testRope = TextDocument.insert 27 "\n" initRope
    let testString = initString.Insert(27, "\n")
    let testRope = TextDocument.insert 207 "\n" testRope
    let testString = testString.Insert(207, "\n")

    let spliString = testString.Split("\n")

    for i = 0 to spliString.Length - 2 do
        Assert.Equal(spliString[i] + "\n", testRope.GetLine i)

    let lastLineIdx = spliString.Length - 1
    let lastLine = TextDocument.getLine lastLineIdx testRope
    Assert.Equal(spliString[lastLineIdx], lastLine)

(* Get line under delete tests. *)
[<Fact>]
let ``Rope.GetLine returns whole string when we delete line break at middle.`` () =
    let rope = TextDocument.create "abcde\nfghij"
    let ropeLine0 = TextDocument.getLine 0 rope
    let ropeLine1 = TextDocument.getLine 1 rope
    Assert.Equal("abcde\n", ropeLine0)
    Assert.Equal("fghij", ropeLine1)

    let rope = TextDocument.delete 5 1 rope
    let ropeLine = TextDocument.getLine 0 rope
    Assert.Equal("abcdefghij", ropeLine)

[<Fact>]
let ``Rope.GetLine returns correct segments when we delete line breaks in complex string`` () =
    let rope = TextDocument.create "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
    
    // Delete first line break and see if we can get expected string from result.
    let rope = TextDocument.delete 11 1 rope
    let ropeLine0 = TextDocument.getLine 0 rope
    let ropeLine1 = TextDocument.getLine 1 rope
    Assert.Equal("Lorem ipsumdolor sit amet,\n", ropeLine0)
    Assert.Equal("consectetur\n", ropeLine1)

    // Delete third line break.
    let rope = TextDocument.delete 38 1 rope
    let ropeLine0 = TextDocument.getLine 0 rope
    let ropeLine1 = TextDocument.getLine 1 rope
    Assert.Equal("Lorem ipsumdolor sit amet,\n", ropeLine0)
    Assert.Equal("consecteturadipiscing elit. \n", ropeLine1)

    // Delete fifth line break
    let rope = TextDocument.delete 71 1 rope
    let ropeLine0 = TextDocument.getLine 0 rope
    let ropeLine1 = TextDocument.getLine 1 rope
    let ropeLine2 = TextDocument.getLine 2 rope
    Assert.Equal("Lorem ipsumdolor sit amet,\n", ropeLine0)
    Assert.Equal("consecteturadipiscing elit. \n", ropeLine1)
    Assert.Equal("Aenean ornare, lacus vitae \n", ropeLine2)

[<Fact>]
let ``Rope.GetLine returns correct segments when we delete multiple line breaks at first half`` () =
    let str = "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
    let rope = TextDocument.create str

    // delete "\ndolor sit amet,\n"
    let rope = TextDocument.delete 11 17 rope
    let str = str.Remove(11, 17)

    // check text is same after deletion (side-check to ensure consistency)
    let ropeText = TextDocument.text rope
    Assert.Equal(str, ropeText)

    let ropeLine0 = TextDocument.getLine 0 rope
    let ropeLine1 = TextDocument.getLine 1 rope
    Assert.Equal("Lorem ipsumconsectetur\n", ropeLine0)
    Assert.Equal("adipiscing elit. \n", ropeLine1)

    // delete "\nlacus vitae \ntempor pretium,\n"
    let rope = TextDocument.delete 57 29 rope
    let str = str.Remove(57, 29)
    let ropeText = TextDocument.text rope
    Assert.Equal(str, ropeText)

    // previos assertions to check they still work
    let ropeLine0 = TextDocument.getLine 0 rope
    let ropeLine1 = TextDocument.getLine 1 rope
    Assert.Equal("Lorem ipsumconsectetur\n", ropeLine0)
    Assert.Equal("adipiscing elit. \n", ropeLine1)

    // current assertion
    let ropeLine2 = TextDocument.getLine 2 rope
    let ropeLine3 = TextDocument.getLine 3 rope
    Assert.Equal("Aenean ornare, \n", ropeLine2)
    Assert.Equal("leo nulla\n", ropeLine3)

    // assertions for lines after to check they still work as expected
    let ropeLine4 = TextDocument.getLine 4 rope
    let ropeLine5 = TextDocument.getLine 5 rope
    let ropeLine6 = TextDocument.getLine 6 rope
    let ropeLine7 = TextDocument.getLine 7 rope
    let ropeLine8 = TextDocument.getLine 8 rope
    Assert.Equal("sollicitudin elit,\n", ropeLine4)
    Assert.Equal("in ultrices mi dui et\n", ropeLine5)
    Assert.Equal("ipsum. Cras condimentum\n", ropeLine6)
    Assert.Equal("purus in metus \n", ropeLine7)
    Assert.Equal("sodales tincidunt. Praesent", ropeLine8)

[<Fact>]
let ``Rope.GetLine returns correct segments when we delete multiple line breaks at last half`` () =
    let rope = TextDocument.create "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"

    // delete "\npurus in metus \n"
    let rope = TextDocument.delete 177 17 rope 
    Assert.Equal("ipsum. Cras condimentumsodales tincidunt. Praesent", rope.GetLine 10)

    // delete " elit,\nin "
    let rope = TextDocument.delete 123 9 rope 
    Assert.Equal("sollicitudin ultrices mi dui et\n", rope.GetLine 8)

    // delete "\ntempor pretium,\nleo nulla"
    let rope = TextDocument.delete 83 25 rope 

    // assert that all lines are as expected
    let ropeLine0 = TextDocument.getLine 0 rope
    let ropeLine1 = TextDocument.getLine 1 rope
    let ropeLine2 = TextDocument.getLine 2 rope
    let ropeLine3 = TextDocument.getLine 3 rope
    let ropeLine4 = TextDocument.getLine 4 rope
    let ropeLine5 = TextDocument.getLine 5 rope
    let ropeLine6 = TextDocument.getLine 6 rope
    let ropeLine7 = TextDocument.getLine 7 rope
    Assert.Equal("Lorem ipsum\n", ropeLine0)
    Assert.Equal("dolor sit amet,\n", ropeLine1)
    Assert.Equal("consectetur\n", ropeLine2)
    Assert.Equal("adipiscing elit. \n", ropeLine3)
    Assert.Equal("Aenean ornare, \n", ropeLine4)
    Assert.Equal("lacus vitulla\n", ropeLine5)
    Assert.Equal("sollicitudin ultrices mi dui et\n", ropeLine6)
    Assert.Equal("ipsum. Cras condimentumsodales tincidunt. Praesent", ropeLine7)

[<Fact>]
let ``Rope.GetLine returns correct segments when we delete multiple line breaks in middle`` () =
    let str = "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
    let rope = TextDocument.create str

    // delete "Aenean ornare, \n"
    let rope = TextDocument.delete 58 16 rope 
    let ropeText = TextDocument.text rope
    let str = str.Remove(58, 16)
    Assert.Equal(str, ropeText)

    // delete "tempor pretium,\nleo nulla\n"
    let rope = TextDocument.delete 71 26 rope 
    let ropeText = TextDocument.text rope
    let str = str.Remove(71, 26)
    Assert.Equal(str, ropeText)

    //delete "dolor sit amet,\n"
    let rope = TextDocument.delete 12 16 rope 
    let ropeText = TextDocument.text rope
    let str = str.Remove(12, 16)
    Assert.Equal(str, ropeText)
    
    // test we retrieve all lines as expected
    let ropeLine0 = TextDocument.getLine 0 rope
    let ropeLine1 = TextDocument.getLine 1 rope
    let ropeLine2 = TextDocument.getLine 2 rope
    let ropeLine3 = TextDocument.getLine 3 rope
    let ropeLine4 = TextDocument.getLine 4 rope
    let ropeLine5 = TextDocument.getLine 5 rope
    let ropeLine6 = TextDocument.getLine 6 rope
    let ropeLine7 = TextDocument.getLine 7 rope
    let ropeLine8 = TextDocument.getLine 8 rope
    Assert.Equal("Lorem ipsum\n", ropeLine0)
    Assert.Equal("consectetur\n", ropeLine1)
    Assert.Equal("adipiscing elit. \n", ropeLine2)
    Assert.Equal("lacus vitae \n", ropeLine3)
    Assert.Equal("sollicitudin elit,\n", ropeLine4)
    Assert.Equal("in ultrices mi dui et\n", ropeLine5)
    Assert.Equal("ipsum. Cras condimentum\n", ropeLine6)
    Assert.Equal("purus in metus \n", ropeLine7)
    Assert.Equal("sodales tincidunt. Praesent", ropeLine8)

