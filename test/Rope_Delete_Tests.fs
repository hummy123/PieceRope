module RopeDeleteTests

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
let ``Can delete from the start of a table's OriginalBuffer`` () =
    let table = TextDocument.delete 0 2 initialTable
    Assert.Equal(text.Substring(2), table.Text())

[<Fact>]
let ``Can delete from the start of a table's AddBuffer`` () =
    let table = 
      TextDocument.create ""
      |> TextDocument.insert 0 text
      |> TextDocument.delete 0 2
    Assert.Equal(text.Substring(2), table.Text())

[<Fact>]
let ``Can delete from the end of a table's OriginalBuffer`` () = 
    let tableText = 
      TextDocument.delete (text.Length - 5) 5 initialTable
      |> TextDocument.text
    Assert.Equal(text.Substring(0, text.Length - 5), tableText)

[<Fact>]
let ``Can delete from the end of a table's AddBuffer`` () =
    let tableText = 
      TextDocument.create ""
      |> TextDocument.insert 0 text
      |> TextDocument.delete (text.Length - 5) 5
      |> TextDocument.text
    Assert.Equal(text.Substring(0, text.Length - 5), tableText)

[<Fact>]
let ``Can delete from the middle of a table's OriginalBuffer`` () = 
    let tableText = 
      TextDocument.delete 1 1 initialTable
      |> TextDocument.text
    let expectedStr = text[0].ToString() + text.Substring(2)
    Assert.Equal(expectedStr, tableText)

[<Fact>]
let ``Can delete from the middle of a table's AddBuffer`` () =
    let tableText =
      TextDocument.delete 1 1 initialTable
      |> TextDocument.text
    let expectedStr = text[0].ToString() + text.Substring(2)
    Assert.Equal(expectedStr, tableText)

[<Fact>]
let ``Can delete when zipper is at start and deletion range includes multiple pieces in a table.`` () =
    let tableText =
      TextDocument.insert 0 insText initialTable
      |> TextDocument.delete 0 10
      |> TextDocument.text
    let expectedStr = (insText + text).Substring(10)
    Assert.Equal(expectedStr, tableText)

[<Fact>]
let ``Can delete from start when zipper is at end`` () =
    let tableText =
      TextDocument.insert text.Length insText initialTable
      |> TextDocument.delete 0 10
      |> TextDocument.text
    let expectedStr = text.Substring(10) + insText
    Assert.Equal(expectedStr, tableText)

[<Fact>]
let ``Can delete in between when zipper is at end`` () =
    let tableText =
      TextDocument.insert text.Length insText initialTable
      |> TextDocument.delete 1 1
      |> TextDocument.text
    let expectedStr = text[0].ToString() + text.Substring(2) + insText
    Assert.Equal(expectedStr, tableText)

[<Fact>]
let ``Can delete at end when zipper is at end`` () =
    let tableText =
      TextDocument.insert text.Length insText initialTable
      |> TextDocument.delete text.Length insText.Length
      |> TextDocument.text
    let expectedStr = text
    Assert.Equal(expectedStr, tableText)

[<Fact>]
let ``Can delete at start when zipper is in middle`` () =
    let tableText =
      TextDocument.insert (text.Length / 2) insText initialTable
      |> TextDocument.delete 0 5
      |> TextDocument.text
    let expectedStr = text.Substring(5, (text.Length/2) - insText.Length) + insText + text.Substring(text.Length/2)
    Assert.Equal(expectedStr, tableText)

[<Fact>]
let ``Can delete middle piece when zipper is in middle`` () =
    let tableText =
      TextDocument.insert (text.Length / 2) insText initialTable
      |> TextDocument.delete (text.Length / 2) insText.Length
      |> TextDocument.text
    let expectedStr = text
    Assert.Equal(expectedStr, tableText)

[<Fact>]
let ``Can delete around (from 1 character before to 1 character after) middle piece when zipper is in middle`` () =
    let tableText =
      TextDocument.insert (text.Length / 2) insText initialTable
      |> TextDocument.delete ((text.Length / 2) - 1) (insText.Length + 2)
      |> TextDocument.text
    let expectedStr = text.Substring(0, (text.Length/2) - 1) + text.Substring((text.Length/2) + 1)
    Assert.Equal(expectedStr, tableText)

[<Fact>]
let ``Can delete at end when zipper is in middle`` () =
    let tableText =
      TextDocument.insert (text.Length / 2) insText initialTable
      |> TextDocument.delete text.Length insText.Length
      |> TextDocument.text
    let expectedStr = text.Substring(0,text.Length/2) + insText + text.Substring(text.Length/2, (text.Length/2) - insText.Length + 1)
    Assert.Equal(expectedStr, tableText)
