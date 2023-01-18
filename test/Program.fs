open PieceRope
open PieceRope.PieceRope
open System

module Program =
    [<EntryPoint>]
    let main _ =  
        let mutable table = PieceRope.empty
        let mutable runningStr = ""
        for i in [0..10] do
            if i = 3 then
                ()
            let halfLength = runningStr.Length / 2
            table <- table.Insert(halfLength, "hello")
            runningStr <- runningStr.Substring(0,halfLength) + "hello" + runningStr.Substring(halfLength)
            let ropeText = table.Text()
            printfn "str:\t %A" runningStr
            printfn "rope:\t %A" <| ropeText
            printfn "are same? %A\n" <| (runningStr = ropeText)
        0