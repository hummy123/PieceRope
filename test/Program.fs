open PieceRope
open PieceRope.PieceRope
open System

[<Literal>]
let text =
    "During the development of the .NET Framework, the class libraries were originally written using a managed code compiler system called \"Simple Managed C\" (SMC)."

[<Literal>]
let insText = "TEST!"

let initialTable = PieceRope.create text

module Program =
    [<EntryPoint>]
    let main _ =  
        let table = initialTable.Insert(5, insText)
        let substring = table.Substring(4, insText.Length + 2)
        printfn "%s" substring
        0