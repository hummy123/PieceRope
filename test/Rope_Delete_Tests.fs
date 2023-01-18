﻿module RopeDeleteTests

open System
open Xunit
open PieceRope
open PieceRope.PieceRope

[<Literal>]
let text =
    "During the development of the .NET Framework, the class libraries were originally written using a managed code compiler system called \"Simple Managed C\" (SMC)."

[<Literal>]
let insText = "TEST!"

let initialTable = PieceRope.create text

[<Fact>]
let ``Can delete from the start of a table's OriginalBuffer`` () =
    let table = initialTable.Delete(0, 2)
    Assert.Equal(text.Substring(2), table.Text())

[<Fact>]
let ``Can delete from the start of a table's AddBuffer`` () =
    let table = PieceRope.create ""
    let table = table.Insert(0, text)
    let table = table.Delete(0, 2)
    Assert.Equal(text.Substring(2), table.Text())

[<Fact>]
let ``Can delete from the end of a table's OriginalBuffer`` () = 
    let table = initialTable.Delete(text.Length - 5, 5)
    Assert.Equal(text.Substring(0, text.Length - 5), table.Text())

[<Fact>]
let ``Can delete from the end of a table's AddBuffer`` () =
    let table = PieceRope.create ""
    let table = table.Insert(0, text)
    let table = table.Delete(text.Length - 5, 5)
    Assert.Equal(text.Substring(0, text.Length - 5), table.Text())

[<Fact>]
let ``Can delete from the middle of a table's OriginalBuffer`` () = 
    let table = initialTable.Delete(1, 1)
    let expectedStr = text[0].ToString() + text.Substring(2)
    Assert.Equal(expectedStr, table.Text())

[<Fact>]
let ``Can delete from the middle of a table's AddBuffer`` () =
    let table = initialTable.Delete(1, 1)
    let expectedStr = text[0].ToString() + text.Substring(2)
    Assert.Equal(expectedStr, table.Text())

[<Fact>]
let ``Can delete when zipper is at start and deletion range includes multiple pieces in a table.`` () =
    let table = initialTable.Insert(0, insText)
    let table = table.Delete(0,10)
    let expectedStr = (insText + text).Substring(10)
    Assert.Equal(expectedStr, table.Text())

[<Fact>]
let ``Can delete from start when zipper is at end`` () =
    let table = initialTable.Insert(text.Length, insText)
    let table = table.Delete(0,10)
    let expectedStr = text.Substring(10) + insText
    Assert.Equal(expectedStr, table.Text())

[<Fact>]
let ``Can delete in between when zipper is at end`` () =
    let table = initialTable.Insert(text.Length, insText)
    let table = table.Delete(1, 1)
    let expectedStr = text[0].ToString() + text.Substring(2) + insText
    Assert.Equal(expectedStr, table.Text())

[<Fact>]
let ``Can delete at end when zipper is at end`` () =
    let table = initialTable.Insert(text.Length, insText)
    let table = table.Delete(text.Length, insText.Length)
    let expectedStr = text
    Assert.Equal(expectedStr, table.Text())

[<Fact>]
let ``Can delete at start when zipper is in middle`` () =
    let table = initialTable.Insert(text.Length/2, insText)
    let table = table.Delete(0,5)
    let expectedStr = text.Substring(5, (text.Length/2) - insText.Length) + insText + text.Substring(text.Length/2)
    Assert.Equal(expectedStr, table.Text())

[<Fact>]
let ``Can delete middle piece when zipper is in middle`` () =
    let table = initialTable.Insert(text.Length/2, insText)
    let table = table.Delete(text.Length/2, insText.Length)
    let expectedStr = text
    Assert.Equal(expectedStr, table.Text())

[<Fact>]
let ``Can delete around (from 1 character before to 1 character after) middle piece when zipper is in middle`` () =
    let table = initialTable.Insert(text.Length/2, insText)
    let table = table.Delete((text.Length/2) - 1, insText.Length + 2)
    let expectedStr = text.Substring(0, (text.Length/2) - 1) + text.Substring((text.Length/2) + 1)
    Assert.Equal(expectedStr, table.Text())

[<Fact>]
let ``Can delete at end when zipper is in middle`` () =
    let table = initialTable.Insert(text.Length/2, insText)
    let table = table.Delete(text.Length, insText.Length)
    let expectedStr = text.Substring(0,text.Length/2) + insText + text.Substring(text.Length/2, (text.Length/2) - insText.Length + 1)
    Assert.Equal(expectedStr, table.Text())