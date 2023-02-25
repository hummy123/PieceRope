module RopeSubstringTests

open System
open Xunit
open HumzApps.TextDocument

[<Literal>]
let text =
    "During the development of the .NET Framework, the class libraries were originally written using a managed code compiler system called \"Simple Managed C\" (SMC)."

[<Literal>]
let insText = "TEST!"

let initialTable = TextDocument.create text

[<Fact>]
let ``Can get a substring from the start of a table's OriginalBuffer`` () =
    let substring = TextDocument.substring 0 2 initialTable
    Assert.Equal("Du", substring)

[<Fact>]
let ``Can get a substring from the whole of a table's AddBuffer`` () =
    let substring =
      TextDocument.insert 5 insText initialTable
      |> TextDocument.substring 5 insText.Length
    Assert.Equal("TEST!", substring)

[<Fact>]
let ``Can get a substring from around a table's AddBuffer`` () =
    let substring =
      TextDocument.insert 5 insText initialTable
      |> TextDocument.substring 4 (insText.Length + 2)
    Assert.Equal("nTEST!g", substring)

[<Fact>]
let ``Can get a substring from the end of a table's OriginalBuffer`` () = 
    let substring = TextDocument.substring (text.Length - 5) 5 initialTable 
    Assert.Equal(text.Substring(text.Length - 5, 5), substring)

[<Fact>]
let ``Can get a substring from the end of a table's AddBuffer`` () =
    let substring = 
      TextDocument.create ""
      |> TextDocument.insert 0 text
      |> TextDocument.substring (text.Length - 5) 5
    Assert.Equal(text.Substring(text.Length - 5, 5), substring)

[<Fact>]
let ``Can get a substring from the middle of a table's OriginalBuffer`` () = 
    let substring = TextDocument.substring 1 1 initialTable
    let expectedStr = "u"
    Assert.Equal(expectedStr, substring)

[<Fact>]
let ``Can get a substring from the middle of a table's AddBuffer`` () =
    let substring = 
      TextDocument.insert 1 "abc" initialTable
      |> TextDocument.substring 2 1
    let expectedStr = "b"
    Assert.Equal(expectedStr, substring)

[<Fact>]
let ``Can get a substring from start when zipper is at end`` () =
    let substring = 
      TextDocument.insert text.Length insText initialTable
      |> TextDocument.substring 0 10
    let expectedStr = text.Substring(0, 10)
    Assert.Equal(expectedStr, substring)

[<Fact>]
let ``Can get a substring from the middle when zipper is at end`` () =
    let substring =
      TextDocument.insert text.Length insText initialTable
      |> TextDocument.substring 10 20
    let expectedStr = text.Substring(10, 20)
    Assert.Equal(expectedStr, substring)

[<Fact>]
let ``Can get a substring from end when zipper is at end`` () =
    let substring =
      TextDocument.insert text.Length insText initialTable
      |> TextDocument.substring text.Length insText.Length
    let expectedStr = insText
    Assert.Equal(expectedStr, substring)

[<Fact>]
let ``Can get a substring from start when zipper is in middle`` () =
    let substring =
      TextDocument.insert (text.Length / 2) insText initialTable
      |> TextDocument.substring 0 5
    let expectedStr = text.Substring(0, 5)
    Assert.Equal(expectedStr, substring)

[<Fact>]
let ``Can get substring of middle when zipper is in middle`` () =
    let substring =
      TextDocument.insert (text.Length / 2) insText initialTable
      |> TextDocument.substring (text.Length / 2) insText.Length
    let expectedStr = insText
    Assert.Equal(expectedStr, substring)

[<Fact>]
let ``Can get a substring over a split piece`` () =
    let substring = 
      TextDocument.create "123456789"
      |> TextDocument.insert 0 "a"
      |> TextDocument.insert 2 "b"
      |> TextDocument.substring 0 3
    Assert.Equal("a1b", substring)

[<Fact>]
let ``TextEdit1`` () =
    // Enter string correctly.
    let table = 
      TextDocument.create "124567893"
      |> TextDocument.insert 8 "98"
      |> TextDocument.insert 9 "ab"

    let expWholestr = "124567899ab83"
    let wholestr = TextDocument.text table
    // Check we have entered string correctly.
    Assert.Equal(expWholestr, wholestr)

    // Get substring.
    let expsubstr9 = "124567899"
    let substr9 = TextDocument.substring 0 9 table
    Assert.Equal(expsubstr9, substr9)

[<Fact>]
let ``TextEdit2`` () =
    // Enter string correctly.
    let table = 
      TextDocument.create "123456789"
      |> TextDocument.insert 8 "a"
      |> TextDocument.insert 9 "b"
      |> TextDocument.insert 7 "c"
      |> TextDocument.insert 8 "d"
      |> TextDocument.insert 6 "e"
      |> TextDocument.insert 7 "f"

    let expWholestr = "123456ef7cd8ab9"
    let wholestr = TextDocument.text table
    // Check we have entered string correctly.
    Assert.Equal(expWholestr, wholestr)

    let expsubstr = "123456e"
    let substr = TextDocument.substring 0 7 table
    Assert.Equal(expsubstr, substr)

    let expsubstr = "123456ef"
    let substr = TextDocument.substring 0 8 table
    Assert.Equal(expsubstr, substr)

    let expsubstr = "123456ef7"
    let substr = TextDocument.substring 0 9 table
    Assert.Equal(expsubstr, substr)
