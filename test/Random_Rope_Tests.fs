module RandomRopeTests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open HumzApps.TextDocument

// Initial data
let lorem = "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
let initRope = TextDocument.create lorem
let initString = lorem

// Generators
let charGen = 
    let chars = Gen.sample 100 100 Arb.generate<char> |> Array.ofList
    new string(chars)

let idxGen maxLen = Gen.choose(0, maxLen) |> Gen.sample 1 1 |> List.head
let lengthGen min max = Gen.choose(min, max) |> Gen.sample 1 1 |> List.head

// Property tests involving insertion
[<Property>] 
let ``String and rope return same text after a series of inputs`` () =
    let mutable testString = initString
    let mutable testRope = initRope

    for i in [0..20] do
        // Generate inputs
        let insStr = charGen
        let idx = idxGen testString.Length
        // Insert and then assert
        testString <- testString.Insert(idx, insStr)
        testRope <- TextDocument.insert idx insStr testRope
        let ropeText = TextDocument.text testRope
        Assert.Equal(testString, ropeText)

[<Property>]
let ``String and rope return same substring after a series of inserts`` () =
    let mutable testString = initString
    let mutable testRope = initRope

    for i in [0..20] do
        // Generate inputs
        let insStr = charGen
        let idx = idxGen testString.Length
        testString <- testString.Insert(idx, insStr)
        testRope <- TextDocument.insert idx insStr testRope

        // Now generate substring ranges
        let startIdx = idxGen testString.Length
        let length = lengthGen 0 (testString.Length - startIdx)

        // Get substrings
        let strSub = testString.Substring(startIdx, length)
        let ropeSub = TextDocument.substring startIdx length testRope

        Assert.Equal(strSub, ropeSub)

[<Property>]
let ``String and rope return same line after a series of newline inserts`` () =
    let mutable testString = initString
    let mutable testRope = initRope

    for i in [0..20] do
        // Generate inputs
        let insStr = "\n"
        let idx = idxGen testString.Length
        testString <- testString.Insert(idx, insStr)
        testRope <- TextDocument.insert idx insStr testRope

        // Split strings by \n so we get number of lines.
        let spliString = testString.Split("\n")

        // Loop over every line number and check if they are same in both.
        // We add \n to the plain string version because we split by \n before.
        for i in [0..spliString.Length - 2] do
            let ropeLine = TextDocument.getLine i testRope
            Assert.Equal(spliString[i] + "\n", ropeLine)

        // Test last line is same in both.
        let lastLineNum = spliString.Length - 1
        let ropeLine = TextDocument.getLine lastLineNum testRope
        Assert.Equal(spliString[lastLineNum], ropeLine)

// Property tests involving deletion
[<Property>]
let ``String and rope return same text after a series of deletions`` () =
    let mutable testString = initString
    let mutable testRope = initRope

    for i in [0..20] do
        // Generate deletion idx and length
        let idx = idxGen <| Math.Max(testString.Length - 1, 0)
        let remainLength = testString.Length - idx
        let length = lengthGen idx <| Math.Max(remainLength, 0)

        let idx = Math.Min(idx, testString.Length - 1)
        let idx = Math.Max(idx, 0)

        let length = Math.Min(length, remainLength)
        let length = Math.Max(length, 0)

        // Delete and then assert
        testString <- testString.Remove(idx, length)
        testRope <- TextDocument.delete idx length testRope
        let ropeText = TextDocument.text testRope
        Assert.Equal(testString, ropeText)

[<Property>]
let ``String and rope return same substring after a series of deletions`` () =
    let mutable testString = initString
    let mutable testRope = initRope

    for i in [0..20] do
        // Generate deletion idx and length
        let idx = idxGen <| Math.Max(testString.Length - 1, 0)
        let remainLength = testString.Length - idx
        let length = lengthGen idx <| Math.Max(remainLength, 0)

        let idx = Math.Min(idx, testString.Length - 1)
        let idx = Math.Max(idx, 0)

        let length = Math.Min(length, remainLength)
        let length = Math.Max(length, 0)

        // Delete 
        testString <- testString.Remove(idx, length)
        testRope <- TextDocument.delete idx length testRope

        // Now generate substring ranges
        let idx = idxGen <| Math.Max(testString.Length - 1, 0)
        let remainLength = testString.Length - idx
        let length = lengthGen 0 <| Math.Max(remainLength, 0)

        let idx = Math.Min(idx, testString.Length - 1)
        let idx = Math.Max(idx, 0)

        // Get substrings
        let strSub = testString.Substring(idx, length)
        let ropeSub = TextDocument.substring idx length testRope

        Assert.Equal(strSub, ropeSub)

[<Property>]
let ``String and rope return same line after a series of deletions`` () =
    let mutable testString = initString
    let mutable testRope = initRope

    for i in [0..20] do
        // Generate deletion idx and length
        let idx = idxGen <| Math.Max(testString.Length - 1, 0)
        let remainLength = testString.Length - idx
        let length = lengthGen idx <| Math.Max(remainLength, 0)

        let idx = Math.Min(idx, testString.Length - 1)
        let idx = Math.Max(idx, 0)

        let length = Math.Min(length, remainLength)
        let length = Math.Max(length, 0)

        // Delete 
        testString <- testString.Remove(idx, length)
        testRope <- TextDocument.delete idx length testRope

        // Now generate substring ranges
        let idx = idxGen <| Math.Max(testString.Length - 1, 0)
        let remainLength = testString.Length - idx
        let length = lengthGen 0 <| Math.Max(remainLength, 0)

        let idx = Math.Min(idx, testString.Length - 1)
        let idx = Math.Max(idx, 0)

        let length = Math.Min(length, remainLength)
        let length = Math.Max(length, 0)

        // Get substrings
        let strSub = testString.Substring(idx, length)
        let ropeSub = TextDocument.substring idx length testRope

        Assert.Equal(strSub, ropeSub)

