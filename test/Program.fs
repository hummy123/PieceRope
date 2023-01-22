open FsCheck
open FsCheck.Xunit
open PieceRope
open PieceRope.PieceRope

// Initial data
let lorem = "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
let initRope = PieceRope.create lorem
let initString = lorem

// Generators
let charGen = 
    let chars = Gen.sample 100 100 Arb.generate<char> |> Array.ofList
    new string(chars)

let idxGen maxLen = Gen.choose(0, maxLen) |> Gen.sample 1 1 |> List.head
let lengthGen min max = Gen.choose(min, max) |> Gen.sample 1 1 |> List.head

// Smallest failing example:
// restart
// idx: 221
// Unhandled exception. System.ArgumentOutOfRangeException: startIndex cannot be larger than length of string. (Parameter 'startIndex')

module Program =
    [<EntryPoint>]
    let main _ =  
        let mutable testString = initString
        let mutable testRope = initRope

        for j in [0..100] do
            printfn "\nrestart"
            testString <- initString
            testRope <- initRope
            for i in [0..20] do
                // Generate inputs
                let insStr = "\n"
                let idx = idxGen testString.Length
                testString <- testString.Insert(idx, insStr)
                testRope <- testRope.Insert(idx, insStr)
            
                printfn "idx: %A" idx

                // Split strings by \n so we get number of lines.
                let spliString = testString.Split("\n")

                // Loop over every line number and check if they are same in both.
                // We add \n to the plain string version because we split by \n before.
                for i in [0..spliString.Length - 2] do
                    if spliString[i] + "\n" <> testRope.GetLine i
                    then printfn "error in loop"

                // Test last line is same in both.
                let lastLineNum = spliString.Length - 1
                if spliString[lastLineNum] <> testRope.GetLine lastLineNum
                then printfn "error in last line"
        0