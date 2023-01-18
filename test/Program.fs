open PieceRope
open PieceRope.PieceRope
open System

module Program =
    [<EntryPoint>]
    let main _ =  
        let str = String.replicate (1024 * 1024) "a"
        printfn "start: %A" DateTime.Now
        let mutable rope = PieceRope.create str
        printfn "finish: %A" DateTime.Now
        rope.Insert(0, str)
        Console.ReadLine()
        0