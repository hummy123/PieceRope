open PieceRope
open PieceRope.PieceRope
open System

module Program =
    [<EntryPoint>]
    let main _ =  
        let mutable table = PieceRope.create ""
        let mutable runningStr = ""
        for i in [0..10] do
            if i = 2 then
                ()
            let halfLength = runningStr.Length / 2
            table <- table.Insert(halfLength, "hello")
            runningStr <- runningStr.Substring(0,halfLength) + "hello" + runningStr.Substring(halfLength)
            printfn "rope:\t %A" <| table.Text()
            printfn "str:\t %A" runningStr
        0