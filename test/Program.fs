open PieceRope
open PieceRope.PieceRope
open System
open FsCheck


// Initial data
let lorem = "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
let initRope = PieceRope.create lorem
let initString = lorem

// minimal failing case: insert
// 27, \n
// 207, \n
// line 12 is different

module Program =
    [<EntryPoint>]
    let main _ =  
        let mutable testString = initString
        let mutable testRope = initRope

        testRope <- testRope.Insert(27, "\n")
        testString <- testString.Insert(27, "\n")

        // no longer same after this insert
        testRope <- testRope.Insert(207, "\n")
        testString <- testString.Insert(207, "\n")

        let spliString = testString.Split("\n")
        let failingLine = testRope.GetLine 12
        printfn "is loop same? %A" (spliString[12] + "\n" = failingLine)

        0