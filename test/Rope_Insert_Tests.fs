module RopeInsertTests

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
let ``Initial table's text returns input text`` () =
    Assert.Equal(text, TextDocument.text initialTable)

[<Fact>]
let ``Can insert into the start of an empty table`` () =
    let ropeText = 
      TextDocument.create ""
      |> TextDocument.insert 0 insText
      |> TextDocument.text
    Assert.Equal(insText, ropeText)

[<Fact>]
let ``Can insert into the start of a table's OriginalBuffer`` () =
    let tableText = 
      TextDocument.insert 0 insText initialTable
      |> TextDocument.text
    Assert.Equal(insText + text, tableText)

[<Fact>]
let ``Can insert into the middle of a table's OriginalBuffer`` () =
    let tableText = 
      TextDocument.insert 3 insText initialTable
      |> TextDocument.text
    let firstStr = text.Substring(0, 3)
    let thirdStr = text.Substring(3)
    let str = firstStr + insText + thirdStr
    Assert.Equal(str, tableText)

[<Fact>]
let ``Can insert into the end of a table's OriginalBuffer`` () =
    let tableText = 
      TextDocument.insert text.Length insText initialTable
      |> TextDocument.text
    let str = text + insText
    Assert.Equal(str, tableText)

[<Fact>]
let ``Can insert into the start of a table's AddBuffer`` () =
    let tableText = 
      TextDocument.create ""
      |> TextDocument.insert 0 text
      |> TextDocument.insert 0 insText
      |> TextDocument.text
    Assert.Equal(insText + text, tableText)

[<Fact>]
let ``Can insert into the middle of a table's AddBuffer`` () =
    let tableText = 
      TextDocument.create ""
      |> TextDocument.insert 0 text
      |> TextDocument.insert 3 insText
      |> TextDocument.text
    let firstStr = text.Substring(0, 3)
    let thirdStr = text.Substring(3)
    let str = firstStr + insText + thirdStr
    Assert.Equal(str, tableText)

[<Fact>]
let ``Can insert into the end of a table's AddBuffer`` () =
    let tableText = 
      TextDocument.create ""
      |> TextDocument.insert 0 text
      |> TextDocument.insert text.Length insText
      |> TextDocument.text
    Assert.Equal(text + insText, tableText)

[<Fact>]
let ``Can continuously insert at start`` () =
    let mutable table = TextDocument.create ""
    let mutable runningStr = ""
    for i in [0..10] do
        table <- TextDocument.insert 0 "hello" table
        let tableText = TextDocument.text table
        runningStr <- runningStr + "hello"
        Assert.Equal(runningStr, tableText)

[<Fact>]
let ``Can continuously insert at middle`` () =
    let mutable table = TextDocument.empty
    let mutable runningStr = ""
    for i in [0..10] do
        let halfLength = runningStr.Length / 2
        table <- TextDocument.insert halfLength "hello" table
        runningStr <- runningStr.Substring(0,halfLength) + "hello" + runningStr.Substring(halfLength)
    let tableText = TextDocument.text table
    Assert.Equal(runningStr, tableText)

[<Fact>]
let ``Can continuously insert at end`` () =
    let mutable table = TextDocument.create ""
    let mutable runningStr = ""
    for i in [0..10] do
        table <- TextDocument.insert runningStr.Length "hello" table
        runningStr <- runningStr + "hello"
    let tableText = TextDocument.text table
    Assert.Equal(runningStr, tableText)

