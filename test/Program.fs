open PieceRope
open PieceRope.PieceRope
open System

module Program =
    [<EntryPoint>]
    let main _ =  
        (* There is an insert error here. *)
        let str = "123456789"
        let table = PieceRope.create "123456789"

        let str = str.Insert(0, "a")
        let table = table.Insert(0, "a")

        let str = str.Insert(2, "b")
        let table = table.Insert(2, "b")
        
        printfn "%s" (table.Text())
        0